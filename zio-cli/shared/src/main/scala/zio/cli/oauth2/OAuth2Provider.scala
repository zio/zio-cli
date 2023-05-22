package zio
package cli
package oauth2

import java.net.URI
import java.net.http.HttpRequest

import zio.json._

/**
 * Provider of OAuth2 authorization.
 */
trait OAuth2Provider {

  /**
   * Name of the provider.
   */
  def name: String

  /**
   * Public client identifier as provided after registration on authorization
   * server. It is used for generating default file name, which holds
   * access token.
   */
  def clientIdentifier: String

  /**
   * Generates HTTP request for authorization request.
   */
  def authorizationRequest(scope: List[String]): HttpRequest

  /**
   * Generates HTTP request for access token request.
   */
  def accessTokenRequest(authorization: AuthorizationResponse): HttpRequest

  /**
   * Generates HTTP request for refresh token request. Returns `None` if this
   * operation is not supported by the provider.
   */
  def refreshTokenRequest(refreshToken: String): Option[HttpRequest]

  /**
   * Converts textual response of authorization request into [[AuthorizationResponse]].
   * Defaults to decoding from standard JSON format, can be overridden if OAuth2 server does
   * not adhere to this standard.
   *
   * @see https://datatracker.ietf.org/doc/html/rfc8628#section-3.2
   *
   * @param body response body
   * @return decoded authorization response
   */
  def decodeAuthorizationResponse(body: String): Either[String, AuthorizationResponse] =
    AuthorizationResponse.authorizationResponseJsonDecoder.decodeJson(body)

  /**
   * Converts textual response of access token request into [[AccessTokenResponse]].
   * Defaults to decoding from standard JSON format, can be overriden if OAuth2 server
   * does not adhere to this standard.
   *
   * @see https://datatracker.ietf.org/doc/html/rfc6749#section-5.1
   * @see https://datatracker.ietf.org/doc/html/rfc6749#section-5.2
   * @see https://datatracker.ietf.org/doc/html/rfc8628#section-3.5
   *
   * @param body response body
   * @return decoded access token response
   */
  def decodeAccessTokenResponse(body: String): Either[String, AccessTokenResponse] =
    AccessTokenResponse.accessTokenResponseJsonDecoder.decodeJson(body)
}

object OAuth2Provider {

  final case class Github(clientId: String) extends OAuth2Provider {
    override val name = "Github"

    override val clientIdentifier = clientId

    override def authorizationRequest(scope: List[String]): HttpRequest =
      HttpRequest
        .newBuilder()
        .uri(URI.create(s"https://github.com/login/device/code?client_id=$clientId&scope=${scope.mkString(",")}"))
        .header("Accept", "application/json")
        .POST(HttpRequest.BodyPublishers.noBody())
        .build()

    override def accessTokenRequest(authorization: AuthorizationResponse): HttpRequest =
      HttpRequest
        .newBuilder()
        .uri(
          URI.create(
            s"https://github.com/login/oauth/access_token?client_id=$clientId&device_code=${authorization.deviceCode}&grant_type=urn:ietf:params:oauth:grant-type:device_code"
          )
        )
        .header("Accept", "application/json")
        .POST(HttpRequest.BodyPublishers.noBody())
        .build()

    override def refreshTokenRequest(refreshToken: String): Option[HttpRequest] = None
  }

  final case class Google(clientId: String, clientSecret: String) extends OAuth2Provider {
    override val name = "Google"

    override val clientIdentifier = clientId

    override def authorizationRequest(scope: List[String]): HttpRequest =
      HttpRequest
        .newBuilder()
        .uri(
          URI.create("https://oauth2.googleapis.com/device/code")
        )
        .headers("Content-Type", "application/x-www-form-urlencoded")
        .POST(HttpRequest.BodyPublishers.ofString(s"client_id=$clientId&scope=${scope.mkString("%20")}"))
        .build()

    override def accessTokenRequest(authorization: AuthorizationResponse): HttpRequest =
      HttpRequest
        .newBuilder()
        .uri(
          URI.create("https://oauth2.googleapis.com/token")
        )
        .headers("Content-Type", "application/x-www-form-urlencoded")
        .POST(
          HttpRequest.BodyPublishers.ofString(
            s"client_id=$clientId&client_secret=$clientSecret&device_code=${authorization.deviceCode}&grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Adevice_code"
          )
        )
        .build()

    override def refreshTokenRequest(refreshToken: String): Option[HttpRequest] =
      Some(
        HttpRequest
          .newBuilder()
          .uri(
            URI.create(s"https://oauth2.googleapis.com/token")
          )
          .headers("Content-Type", "application/x-www-form-urlencoded")
          .POST(
            HttpRequest.BodyPublishers.ofString(
              s"client_id=$clientId&client_secret=$clientSecret&refresh_token=$refreshToken&grant_type=refresh_token"
            )
          )
          .build()
      )

    override def decodeAuthorizationResponse(body: String): Either[String, AuthorizationResponse] =
      Google.gAuthorizationResponseDecoder
        .decodeJson(body)
        .map(g =>
          AuthorizationResponse(
            g.deviceCode,
            g.userCode,
            g.verificationUrl,
            None,
            Duration.fromSeconds(g.expiresIn),
            Duration.fromSeconds(g.interval)
          )
        )
  }

  object Google {
    @jsonMemberNames(SnakeCase)
    final case class GAuthorizationResponse(
      deviceCode: String,
      userCode: String,
      verificationUrl: String,
      expiresIn: Long,
      interval: Long
    )

    val gAuthorizationResponseDecoder: JsonDecoder[GAuthorizationResponse] =
      DeriveJsonDecoder.gen[GAuthorizationResponse]
  }

  final case class Facebook(appId: String, clientToken: String) extends OAuth2Provider {
    override val name = "Facebook"

    override val clientIdentifier = appId

    val fbAccessToken = s"$appId%7C$clientToken"

    override def authorizationRequest(scope: List[String]): HttpRequest =
      HttpRequest
        .newBuilder()
        .uri(
          URI.create(
            s"https://graph.facebook.com/v2.6/device/login?access_token=$fbAccessToken&scope=${scope.mkString(",")}"
          )
        )
        .POST(HttpRequest.BodyPublishers.noBody())
        .build()

    override def accessTokenRequest(authorization: AuthorizationResponse): HttpRequest =
      HttpRequest
        .newBuilder()
        .uri(
          URI.create(
            s"https://graph.facebook.com/v2.6/device/login_status?access_token=$fbAccessToken&code=${authorization.deviceCode}"
          )
        )
        .POST(HttpRequest.BodyPublishers.noBody())
        .build()

    override def refreshTokenRequest(refreshToken: String): Option[HttpRequest] = None

    override def decodeAuthorizationResponse(body: String): Either[String, AuthorizationResponse] =
      Facebook.fbAuthorizationResponseDecoder
        .decodeJson(body)
        .map(fb =>
          AuthorizationResponse(
            fb.code,
            fb.userCode,
            fb.verificationUri,
            None,
            Duration.fromSeconds(fb.expiresIn),
            Duration.fromSeconds(fb.interval)
          )
        )

    override def decodeAccessTokenResponse(body: String): Either[String, AccessTokenResponse] = {
      val decodeSuccess: Either[String, AccessTokenResponse] =
        Facebook.fbAccessTokenResponse
          .decodeJson(body)
          .map(fb =>
            AccessTokenResponse
              .AccessToken(fb.accessToken, TokenType.Bearer, Some(Duration.fromSeconds(fb.expiresIn)), None, List.empty)
          )

      val decodeError: Either[String, AccessTokenResponse] =
        Facebook.fbAccessTokenError
          .decodeJson(body)
          .map { fb =>
            val kind = fb.error.errorSubcode match {
              case 1349174 => AccessTokenResponse.Error.Kind.AuthorizationPending
              case 1349172 => AccessTokenResponse.Error.Kind.SlowDown
              case 1349152 => AccessTokenResponse.Error.Kind.ExpiredToken
              case 1349175 => AccessTokenResponse.Error.Kind.InvalidRequest
              case _       => AccessTokenResponse.Error.Kind.Other(fb.error.message)
            }

            AccessTokenResponse
              .Error(kind, Some(fb.error.errorUserMsg), None, None)
          }

      // 2.12-friendly decodeError.orElse(decodeSuccess)
      decodeError match {
        case r @ Right(_) => r
        case _            => decodeSuccess
      }
    }
  }

  object Facebook {
    @jsonMemberNames(SnakeCase)
    final case class FBAuthorizationResponse(
      code: String,
      userCode: String,
      verificationUri: String,
      expiresIn: Long,
      interval: Long
    )

    @jsonMemberNames(SnakeCase)
    final case class FBAccessTokenResponse(
      accessToken: String,
      expiresIn: Long
    )

    final case class FBAccessTokenError(error: FBAccessTokenErrorContent)

    @jsonMemberNames(SnakeCase)
    final case class FBAccessTokenErrorContent(
      message: String,
      errorSubcode: Long,
      errorUserMsg: String
    )

    val fbAuthorizationResponseDecoder: JsonDecoder[FBAuthorizationResponse] =
      DeriveJsonDecoder.gen[FBAuthorizationResponse]

    val fbAccessTokenResponse: JsonDecoder[FBAccessTokenResponse] =
      DeriveJsonDecoder.gen[FBAccessTokenResponse]

    implicit val fbAccessTokenErrorContent: JsonDecoder[FBAccessTokenErrorContent] =
      DeriveJsonDecoder.gen[FBAccessTokenErrorContent]

    val fbAccessTokenError: JsonDecoder[FBAccessTokenError] =
      DeriveJsonDecoder.gen[FBAccessTokenError]

  }
}
