package zio
package cli
package oauth2

import java.net.URI
import java.net.http.HttpRequest

import zio.json._

/**
 * An OAuth 2.0 authorization server that supports the device authorization grant of RFC 8628. A provider builds the
 * HTTP requests of the flow and decodes their responses; `Options.oauth2`, available on the JVM, sends them and keeps
 * the resulting token. `OAuth2Provider.Github`, `OAuth2Provider.Google` and `OAuth2Provider.Facebook` are ready-made;
 * implement this trait for another server.
 *
 * {{{
 * val github: OAuth2Provider       = OAuth2Provider.Github("Iv1.8a61f9b3a7aba766")
 * val token: Options[OAuth2Token] = Options.oauth2(github, List("repo", "read:user"))
 * }}}
 *
 * @see
 *   https://datatracker.ietf.org/doc/html/rfc8628
 */
trait OAuth2Provider {

  /**
   * Name of the provider. The help text of a command with an `Options.oauth2` option shows it, and the default token
   * file name contains it in lower case.
   */
  def name: String

  /**
   * Public client identifier as provided after registration on the authorization server. It is used for generating the
   * default name of the file that holds the access token, in the user's home directory: it is `oauth2_`, the `name` in
   * lower case, `_`, the client identifier and `_access_token.json`, such as
   * `oauth2_github_Iv1.8a61f9b3a7aba766_access_token.json` for `Github("Iv1.8a61f9b3a7aba766")`.
   */
  def clientIdentifier: String

  /**
   * Builds the device authorization request, which asks for the permissions in `scope`. Its response body is read with
   * `decodeAuthorizationResponse`.
   */
  def authorizationRequest(scope: List[String]): HttpRequest

  /**
   * Builds the token request that polls for the access token with the device code of `authorization`. `Options.oauth2`
   * sends it again while the response is an `authorization_pending` or `slow_down` error, until
   * `authorization.expiresIn` has passed. Its response body is read with `decodeAccessTokenResponse`.
   */
  def accessTokenRequest(authorization: AuthorizationResponse): HttpRequest

  /**
   * Builds the request that exchanges `refreshToken` for a new access token. Returns `None` if the provider does not
   * support refresh tokens, as `OAuth2Provider.Facebook` does. Its response body is read with
   * `decodeAccessTokenResponse`.
   */
  def refreshTokenRequest(refreshToken: String): Option[HttpRequest]

  /**
   * Converts the response body of the authorization request into an [[AuthorizationResponse]]. The default decodes the
   * standard JSON format with `AuthorizationResponse.authorizationResponseJsonDecoder`; override it if the OAuth2
   * server does not follow this standard, as `OAuth2Provider.Google` and `OAuth2Provider.Facebook` do.
   *
   * @see
   *   https://datatracker.ietf.org/doc/html/rfc8628#section-3.2
   *
   * @param body
   *   response body
   * @return
   *   decoded authorization response
   */
  def decodeAuthorizationResponse(body: String): Either[String, AuthorizationResponse] =
    AuthorizationResponse.authorizationResponseJsonDecoder.decodeJson(body)

  /**
   * Converts the response body of an access token or refresh token request into an [[AccessTokenResponse]]. The default
   * decodes the standard JSON format with `AccessTokenResponse.accessTokenResponseJsonDecoder`; override it if the
   * OAuth2 server does not follow this standard, as `OAuth2Provider.Facebook` does.
   *
   * @see
   *   https://datatracker.ietf.org/doc/html/rfc6749#section-5.1
   * @see
   *   https://datatracker.ietf.org/doc/html/rfc6749#section-5.2
   * @see
   *   https://datatracker.ietf.org/doc/html/rfc8628#section-3.5
   *
   * @param body
   *   response body
   * @return
   *   decoded access token response
   */
  def decodeAccessTokenResponse(body: String): Either[String, AccessTokenResponse] =
    AccessTokenResponse.accessTokenResponseJsonDecoder.decodeJson(body)
}

/**
 * Ready-made providers: `Github`, `Google` and `Facebook`.
 */
object OAuth2Provider {

  /**
   * GitHub's device flow for the app with the client ID `clientId`. Each request is a POST with an empty body, the
   * parameters in the query string and the header `Accept: application/json`: the authorization request goes to
   * `https://github.com/login/device/code`, with the scopes joined by commas, and the token and refresh requests go to
   * `https://github.com/login/oauth/access_token`. Refresh tokens are supported, and responses are read with the
   * standard decoders.
   *
   * {{{
   * val github = OAuth2Provider.Github("Iv1.8a61f9b3a7aba766")
   * github.authorizationRequest(List("repo", "gist")).uri()
   * // https://github.com/login/device/code?client_id=Iv1.8a61f9b3a7aba766&scope=repo,gist
   * }}}
   */
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

    override def refreshTokenRequest(refreshToken: String): Option[HttpRequest] =
      Some(
        HttpRequest
          .newBuilder()
          .uri(
            URI.create(
              s"https://github.com/login/oauth/access_token?client_id=$clientId&grant_type=refresh_token&refresh_token=$refreshToken"
            )
          )
          .header("Accept", "application/json")
          .POST(HttpRequest.BodyPublishers.noBody())
          .build()
      )
  }

  /**
   * Google's device flow for the client `clientId` with the secret `clientSecret`. Each request is a POST with a
   * form-encoded body: the authorization request goes to `https://oauth2.googleapis.com/device/code`, with the scopes
   * joined by `%20`, and the token and refresh requests go to `https://oauth2.googleapis.com/token` and include
   * `clientSecret`. Refresh tokens are supported.
   *
   * The authorization response is read with `Google.gAuthorizationResponseDecoder`, which takes the URI from the field
   * `verification_url` instead of `verification_uri` and needs the `interval` field. Token responses are read with the
   * standard decoder.
   */
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

  /**
   * Google's device authorization response and its decoder.
   */
  object Google {

    /**
     * Google's device authorization response. The JSON fields are `device_code`, `user_code`, `verification_url`,
     * `expires_in` and `interval`, all required; `expiresIn` and `interval` are in seconds.
     */
    @jsonMemberNames(SnakeCase)
    final case class GAuthorizationResponse(
      deviceCode: String,
      userCode: String,
      verificationUrl: String,
      expiresIn: Long,
      interval: Long
    )

    /**
     * Decodes a `GAuthorizationResponse`. `Google#decodeAuthorizationResponse` uses it.
     */
    val gAuthorizationResponseDecoder: JsonDecoder[GAuthorizationResponse] =
      DeriveJsonDecoder.gen[GAuthorizationResponse]
  }

  /**
   * Facebook's device login for the app `appId` with the client token `clientToken`. Each request is a POST with an
   * empty body and `fbAccessToken` as the `access_token` query parameter: the authorization request goes to
   * `https://graph.facebook.com/v2.6/device/login`, with the scopes joined by commas, and the token request goes to
   * `https://graph.facebook.com/v2.6/device/login_status`. Refresh tokens are not supported: `refreshTokenRequest`
   * returns `None`.
   *
   * The decoders read Facebook's formats. The device code comes from the `code` field. A successful token response,
   * with `access_token` and `expires_in`, gives a `Bearer` token with no refresh token and an empty scope. In an error
   * response, `error_subcode` 1349174 gives `AuthorizationPending`, 1349172 gives `SlowDown`, 1349152 gives
   * `ExpiredToken`, 1349175 gives `InvalidRequest`, and other values give `Other` with the error's `message`; the
   * error's `error_user_msg` becomes the error description.
   */
  final case class Facebook(appId: String, clientToken: String) extends OAuth2Provider {
    override val name = "Facebook"

    override val clientIdentifier = appId

    /**
     * The value of the `access_token` query parameter: `appId` and `clientToken` joined by `|`, which is written
     * URL-encoded as `%7C` (for example `123456%7Cabc`).
     */
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

  /**
   * Facebook's response formats and their decoders.
   */
  object Facebook {

    /**
     * Facebook's device login response. The JSON fields are `code` (the device code), `user_code`, `verification_uri`,
     * `expires_in` and `interval`, all required; `expiresIn` and `interval` are in seconds.
     */
    @jsonMemberNames(SnakeCase)
    final case class FBAuthorizationResponse(
      code: String,
      userCode: String,
      verificationUri: String,
      expiresIn: Long,
      interval: Long
    )

    /**
     * Facebook's successful login status response. The JSON fields are `access_token` and `expires_in` (in seconds),
     * both required.
     */
    @jsonMemberNames(SnakeCase)
    final case class FBAccessTokenResponse(
      accessToken: String,
      expiresIn: Long
    )

    /**
     * Facebook's error response: a JSON object whose `error` field holds the details.
     */
    final case class FBAccessTokenError(error: FBAccessTokenErrorContent)

    /**
     * The details of a Facebook error. The JSON fields are `message`, `error_subcode` and `error_user_msg`, all
     * required. When one of them is missing, `Facebook#decodeAccessTokenResponse` does not read the body as an error
     * and tries the success format instead.
     */
    @jsonMemberNames(SnakeCase)
    final case class FBAccessTokenErrorContent(
      message: String,
      errorSubcode: Long,
      errorUserMsg: String
    )

    /**
     * Decodes an `FBAuthorizationResponse`. `Facebook#decodeAuthorizationResponse` uses it.
     */
    val fbAuthorizationResponseDecoder: JsonDecoder[FBAuthorizationResponse] =
      DeriveJsonDecoder.gen[FBAuthorizationResponse]

    /**
     * Decodes an `FBAccessTokenResponse`. `Facebook#decodeAccessTokenResponse` uses it.
     */
    val fbAccessTokenResponse: JsonDecoder[FBAccessTokenResponse] =
      DeriveJsonDecoder.gen[FBAccessTokenResponse]

    /**
     * Decodes an `FBAccessTokenErrorContent`; `fbAccessTokenError` uses it for the `error` field.
     */
    implicit val fbAccessTokenErrorContent: JsonDecoder[FBAccessTokenErrorContent] =
      DeriveJsonDecoder.gen[FBAccessTokenErrorContent]

    /**
     * Decodes an `FBAccessTokenError`. `Facebook#decodeAccessTokenResponse` tries it before `fbAccessTokenResponse`.
     */
    val fbAccessTokenError: JsonDecoder[FBAccessTokenError] =
      DeriveJsonDecoder.gen[FBAccessTokenError]

  }
}
