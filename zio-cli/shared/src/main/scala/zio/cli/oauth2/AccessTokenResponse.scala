package zio
package cli
package oauth2

import zio.json.{jsonMemberNames, DeriveJsonDecoder, JsonDecoder, SnakeCase}

/**
 * The response of the token endpoint, received while polling during device authorization or after a refresh request. It
 * is either [[AccessTokenResponse.AccessToken]] when the server issued a token, or [[AccessTokenResponse.Error]] when
 * it returned an error, which includes `authorization_pending` and `slow_down` while the user has not finished yet.
 *
 * @see
 *   https://datatracker.ietf.org/doc/html/rfc6749#section-5
 */
sealed trait AccessTokenResponse extends Serializable with Product

/**
 * The cases of `AccessTokenResponse` and its JSON decoder.
 */
object AccessTokenResponse {

  /**
   * A successful response, carrying the access token issued by the authorization server.
   *
   * @see
   *   https://datatracker.ietf.org/doc/html/rfc6749#section-5.1
   *
   * @param accessToken
   *   access token
   * @param tokenType
   *   type of token
   * @param expiresIn
   *   lifetime of the access token, from the `expires_in` field in seconds; `None` when the field is missing
   * @param refreshToken
   *   refresh token for obtaining a new access token
   * @param scope
   *   the `scope` field split at each comma and each space; empty when the field is missing. An empty `scope` string
   *   gives `List("")`, and `"repo, gist"` gives `List("repo", "", "gist")`.
   */
  final case class AccessToken(
    accessToken: String,
    tokenType: TokenType,
    expiresIn: Option[Duration],
    refreshToken: Option[String],
    scope: List[String]
  ) extends AccessTokenResponse

  /**
   * An error response: the authorization failed, or it is still in progress (`authorization_pending` and `slow_down`).
   *
   * @see
   *   https://datatracker.ietf.org/doc/html/rfc6749#section-5.2
   *
   * @param error
   *   kind of error
   * @param errorDescription
   *   human-readable textual expression of the error
   * @param errorUri
   *   URI to additional information about the error
   * @param interval
   *   the `interval` field in seconds, if the server sent one (for example with `slow_down`)
   */
  final case class Error(
    error: Error.Kind,
    errorDescription: Option[String],
    errorUri: Option[String],
    interval: Option[Duration]
  ) extends AccessTokenResponse

  /**
   * Companion of `Error`, holding the error kinds.
   */
  object Error {

    /**
     * The `error` code of an `AccessTokenResponse.Error`. Codes that the decoder does not know become `Kind.Other`.
     *
     * @see
     *   https://datatracker.ietf.org/doc/html/rfc6749#section-5.2
     * @see
     *   https://datatracker.ietf.org/doc/html/rfc8628#section-3.5
     */
    sealed trait Kind extends Serializable with Product

    /**
     * The error codes of RFC 6749 section 5.2 and RFC 8628 section 3.5, and `Other` for the rest.
     */
    object Kind {

      /**
       * `invalid_request`: the request is missing a required parameter, has an unsupported parameter value, or is
       * otherwise malformed.
       */
      case object InvalidRequest extends Kind

      /**
       * `invalid_client`: client authentication failed.
       */
      case object InvalidClient extends Kind

      /**
       * `invalid_grant`: the grant (such as the device code) or the refresh token is invalid, expired or revoked.
       */
      case object InvalidGrant extends Kind

      /**
       * `unauthorized_client`: the client is not allowed to use this grant type.
       */
      case object UnauthorizedClient extends Kind

      /**
       * `unsupported_grant_type`: the authorization server does not support the grant type.
       */
      case object UnsupportedGrantType extends Kind

      /**
       * `invalid_scope`: the requested scope is invalid, unknown or malformed, or exceeds the scope granted.
       */
      case object InvalidScope extends Kind

      /**
       * `authorization_pending`: the user has not completed the authorization yet. `Options.oauth2` keeps polling when
       * it receives this error.
       */
      case object AuthorizationPending extends Kind

      /**
       * `slow_down`: the authorization is still pending and the client should poll less often. `Options.oauth2` keeps
       * polling, with a longer delay, when it receives this error.
       */
      case object SlowDown extends Kind

      /**
       * `access_denied`: the user denied the authorization request.
       */
      case object AccessDenied extends Kind

      /**
       * `expired_token`: the device code has expired, so a new device authorization request is needed.
       */
      case object ExpiredToken extends Kind

      /**
       * An error code that is not one of the ten known codes, kept as received in `code`. The match is case-sensitive:
       * `"Slow_Down"` decodes to `Other("Slow_Down")`.
       */
      final case class Other(code: String) extends Kind

      /**
       * Decodes the JSON string of the `error` field: each of the ten known codes, matched case-sensitively, gives its
       * case object, and any other string gives `Other` with that string.
       */
      implicit val kindJsonDecoder: JsonDecoder[Kind] =
        JsonDecoder.string.map {
          case "invalid_request"        => InvalidRequest
          case "invalid_client"         => InvalidClient
          case "invalid_grant"          => InvalidGrant
          case "unauthorized_client"    => UnauthorizedClient
          case "unsupported_grant_type" => UnsupportedGrantType
          case "invalid_scope"          => InvalidScope
          case "authorization_pending"  => AuthorizationPending
          case "slow_down"              => SlowDown
          case "access_denied"          => AccessDenied
          case "expired_token"          => ExpiredToken
          case code                     => Other(code)
        }
    }
  }

  @jsonMemberNames(SnakeCase)
  private[this] final case class AccessTokenRaw(
    accessToken: String,
    tokenType: TokenType,
    expiresIn: Option[Long],
    refreshToken: Option[String],
    scope: Option[String]
  )

  @jsonMemberNames(SnakeCase)
  private final case class ErrorRaw(
    error: Error.Kind,
    errorDescription: Option[String],
    errorUri: Option[String],
    interval: Option[Long]
  )

  private[this] val errorJsonDecoder: JsonDecoder[AccessTokenResponse] =
    DeriveJsonDecoder
      .gen[ErrorRaw]
      .map(raw =>
        Error(
          raw.error,
          raw.errorDescription,
          raw.errorUri,
          raw.interval.map(Duration.fromSeconds)
        )
      )
      .widen

  private[this] val accessTokenJsonDecoder: JsonDecoder[AccessTokenResponse] =
    DeriveJsonDecoder
      .gen[AccessTokenRaw]
      .map(raw =>
        AccessToken(
          raw.accessToken,
          raw.tokenType,
          raw.expiresIn.map(Duration.fromSeconds),
          raw.refreshToken,
          raw.scope.map(_.split(",| ").toList).getOrElse(List.empty)
        )
      )
      .widen

  /**
   * Decodes the JSON body of a token endpoint response. It first tries the error form, which needs an `error` field,
   * and returns an `Error`; otherwise it decodes the success form, which needs `access_token` and `token_type`, into an
   * `AccessToken`. `expires_in` and `interval` are read as seconds, and `scope` is split at each comma and each space.
   * When neither form matches, the message is the one of the success form, such as `.access_token(missing)`.
   *
   * {{{
   * val decoder = AccessTokenResponse.accessTokenResponseJsonDecoder
   *
   * decoder.decodeJson("""{"access_token":"gho_16C7e42F","token_type":"bearer","scope":"repo,gist"}""")
   * // Right(AccessToken(gho_16C7e42F,Bearer,None,None,List(repo, gist)))
   * decoder.decodeJson("""{"error":"slow_down","interval":10}""")
   * // Right(Error(SlowDown,None,None,Some(PT10S)))
   * decoder.decodeJson("""{"token_type":"bearer"}""")
   * // Left(.access_token(missing))
   * }}}
   */
  val accessTokenResponseJsonDecoder: JsonDecoder[AccessTokenResponse] =
    errorJsonDecoder <> accessTokenJsonDecoder
}
