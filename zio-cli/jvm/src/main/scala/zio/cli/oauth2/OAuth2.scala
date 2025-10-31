package zio
package cli
package oauth2

import java.net.http.{HttpClient, HttpResponse}
import java.nio.file.Path

import zio.json._

private[cli] class OAuth2(provider: OAuth2Provider, file: Path, scope: List[String]) {
  val client = HttpClient.newBuilder().build()

  /**
   * Returns [[OAuth2Token]] either loaded from file (if available) or by authorizing on `provider`s server.
   */
  def loadOrAuthorize: Task[OAuth2Token] =
    Clock.currentDateTime.flatMap { time =>
      loadFromFile.flatMap {
        // Not expired
        case Some(accessToken) if accessToken.expiresAt.forall(_.isAfter(time)) => ZIO.succeed(accessToken)
        // Expired
        case Some(accessToken) => refreshAccessToken(accessToken)
        // Not existing
        case _ => authorize
      }.flatMap(makeOAuth2Token)
    }

  /**
   * Performs OAuth2 authorization, returns access token as received from the authorization server.
   */
  def authorize: Task[AccessToken] =
    requestAuthorization.tap(informUser).flatMap(waitForAccessToken)

  /**
   * Attempts to load recent access token from file; returns `None` if the file could not be found, could not be
   * decrypted or could not be decoded.
   */
  def loadFromFile: Task[Option[AccessToken]] =
    ZIO.readFile(file).option.map(_.flatMap(_.fromJson[AccessToken].toOption))

  /**
   * Creates [[OAuth2Token]] object from original access token (either loaded from file or freshly obtained from
   * authorization server).
   *
   * @param initialToken
   *   original access token
   */
  def makeOAuth2Token(initialToken: AccessToken): UIO[OAuth2Token] =
    Ref.Synchronized
      .make(initialToken)
      .map(ref =>
        new OAuth2Token {
          val refreshTokenNow = ref.updateZIO(refreshAccessToken)
          val accessToken     =
            ref.updateAndGetZIO { acc =>
              Clock.currentDateTime.flatMap { time =>
                acc.expiresAt.map(_.isBefore(time)) match {
                  // Expired
                  case Some(true) => refreshAccessToken(acc)
                  // No expiration or not yet expired
                  case _ => ZIO.succeed(acc)
                }
              }
            }
        }
      )

  /**
   * Refreshes access token, either by re-authorizing or by using refresh token, if available.
   *
   * @param accessToken
   *   original access token
   */
  def refreshAccessToken(accessToken: AccessToken): Task[AccessToken] =
    accessToken.refreshToken.fold(authorize)(requestRefreshToken)

  /**
   * Sends to an authorization server request for authorization.
   */
  def requestAuthorization: Task[AuthorizationResponse] =
    ZIO
      .fromCompletableFuture(
        client.sendAsync(
          provider.authorizationRequest(scope),
          HttpResponse.BodyHandlers.ofString()
        )
      )
      .flatMap(response =>
        if (response.statusCode() == 200) {
          ZIO
            .fromEither(provider.decodeAuthorizationResponse(response.body()))
            .mapError(e => new Exception(s"Response of authorization request could not be decoded: $e"))
        } else {
          ZIO.fail(
            new Exception(s"Authorization server returned error after authorization request: ${response.body()}")
          )
        }
      )

  def waitForAccessToken(response: AuthorizationResponse): Task[AccessToken] =
    ZIO
      .fromCompletableFuture(
        client.sendAsync(
          provider.accessTokenRequest(response),
          HttpResponse.BodyHandlers.ofString()
        )
      )
      .flatMap(response =>
        ZIO.fromEither(
          provider
            .decodeAccessTokenResponse(response.body())
            .left
            .map(e => new Exception(s"Response of access token request could not be decoded: $e"))
        )
      )
      .repeat(pollingSchedule(response.interval, response.expiresIn))
      .flatMap(res => processResponse(res, None))

  /**
   * Creates ZIO Schedule for regular polling of authentication server until the response indicates success, failure or
   * time has expired.
   *
   * Initial interval is `interval`. This interval can increase by `slow_down` requests from server. The schedule will
   * terminate when `expiresIn` has elapsed.
   *
   * @see
   *   https://datatracker.ietf.org/doc/html/rfc8628#section-3.3
   * @see
   *   https://datatracker.ietf.org/doc/html/rfc8628#section-3.5
   *
   * @param interval
   *   initial polling interval
   * @param expiresIn
   *   maximum period
   * @return
   *   the last response received before terminating
   */
  def pollingSchedule(
    interval: Duration,
    expiresIn: Duration
  ): Schedule[Any, AccessTokenResponse, AccessTokenResponse] = {
    import AccessTokenResponse._
    import AccessTokenResponse.Error.Kind._

    // Generate delays for each poll attempt. It starts with `interval`.
    // Every `slow_down` will increase delay of this round and of
    // all future rounds by given interval.
    val delays: Schedule[Any, AccessTokenResponse, Duration] =
      Schedule
        .identity[AccessTokenResponse]
        .map {
          case e: Error if e.error == SlowDown =>
            // Delay is added only when `slow_down` was requested.
            e.interval.orElse(Some(5.seconds))
          case _ => None
        }
        // Add additional delay to this round if required.
        .addDelay(_.getOrElse(Duration.Zero))
        // Increase delays to following rounds if required.
        .fold(interval) {
          case (duration, Some(interval)) => duration + interval
          case (duration, _)              => duration
        }

    Schedule.delayed(delays) *>
      Schedule
        .recurWhile[AccessTokenResponse](_ match {
          case e: Error =>
            e.error == AuthorizationPending || e.error == SlowDown
          case _ => false
        })
        .upTo(expiresIn)
  }

  /**
   * Sends to an authorization server request to refresh access token using a refresh token.
   */
  def requestRefreshToken(refreshToken: String): Task[AccessToken] =
    ZIO
      .fromEither(
        provider
          .refreshTokenRequest(refreshToken)
          .toRight(new Exception("Authorization server does not support refresh tokens."))
      )
      .flatMap(request => ZIO.fromCompletableFuture(client.sendAsync(request, HttpResponse.BodyHandlers.ofString())))
      .flatMap(response =>
        if (response.statusCode() == 200) {
          ZIO
            .fromEither(
              provider
                .decodeAccessTokenResponse(response.body())
                .left
                .map(e => new Exception(s"Response of refresh token request could not be decoded: $e"))
            )
        } else {
          ZIO.fail(
            new Exception(s"Authorization server returned error after refresh token request: ${response.body()}")
          )
        }
      )
      .flatMap(res => processResponse(res, Some(refreshToken)))

  /**
   * Converts standard access token response into [[AccessToken]] and saves it to file. If response contained an error,
   * it will be
   */
  def processResponse(response: AccessTokenResponse, refreshToken: Option[String]): Task[AccessToken] =
    response match {
      case e: AccessTokenResponse.Error =>
        val description = e.errorDescription.map(d => s" and with message: '$d'").getOrElse("")
        val uri         = e.errorUri.map(uri => s" For more information visit: $uri").getOrElse("")

        ZIO.fail(new Exception(s"Authorization server returned error of type ${e.error}$description.$uri"))
      case t: AccessTokenResponse.AccessToken =>
        Clock.currentDateTime
          .map(time =>
            AccessToken(
              t.accessToken,
              t.tokenType,
              t.expiresIn.map(exp => time.plus(exp)),
              t.refreshToken.orElse(refreshToken),
              t.scope
            )
          )
          .tap(token => ZIO.writeFile(file, token.toJson))
    }

  /**
   * Prints an information box with instructions how to confirm authorization request.
   */
  def informUser(response: AuthorizationResponse): UIO[Unit] =
    Clock.localDateTime.map { time =>
      val boxUrlLength  = response.verificationUri.length() + 2
      val boxCodeLength = response.userCode.length() + 2
      val expiresIn     = response.expiresIn.toMinutes()
      val expiresAt     = time.plus(response.expiresIn).withNano(0).toLocalTime()

      s"""| >>
          | >>  Application requests to perform OAuth2
          | >>  authorization.
          | >>
          | >>  Visit following URL in a browser:
          | >>
          | >>   ┏${"━" * boxUrlLength}┓
          | >>   ┃ ${response.verificationUri} ┃
          | >>   ┗${"━" * boxUrlLength}┛
          | >>
          | >>  And enter following code:
          | >>
          | >>   ┏${"━" * boxCodeLength}┓
          | >>   ┃ ${response.userCode} ┃
          | >>   ┗${"━" * boxCodeLength}┛
          | >>
          | >>  Code will expire in $expiresIn minutes at $expiresAt.
          | >>
""".stripMargin
    }
      .flatMap(text => Console.printLine(s"${text}\nWaiting...").orDie)

}
