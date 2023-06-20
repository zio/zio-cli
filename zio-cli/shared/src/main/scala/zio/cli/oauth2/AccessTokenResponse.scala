package zio
package cli
package oauth2

import zio.json.{jsonMemberNames, DeriveJsonDecoder, JsonDecoder, SnakeCase}

/**
 * Response to access token request informing about progress of device authorization. The response can be either
 * [[AccessTokenResponse.AccessToken]] in case of successful termination or [[AccessTokenResponse.Error]] when any error
 * occurrs.
 *
 * @see
 *   https://datatracker.ietf.org/doc/html/rfc6749#section-5
 */
sealed trait AccessTokenResponse extends Serializable with Product

object AccessTokenResponse {

  /**
   * Result of successful authorization process providing access token issued by the authorization server.
   *
   * @see
   *   https://datatracker.ietf.org/doc/html/rfc6749#section-5.1
   *
   * @param accessToken
   *   access token
   * @param tokenType
   *   type of token
   * @param expiresIn
   *   lifetime of access token
   * @param refreshToken
   *   refresh token for obtaining new access token
   * @param scope
   *   scope of access token
   */
  final case class AccessToken(
    accessToken: String,
    tokenType: TokenType,
    expiresIn: Option[Duration],
    refreshToken: Option[String],
    scope: List[String]
  ) extends AccessTokenResponse

  /**
   * Result of unsuccessful or still ongoing authorization process.
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
   *   minimum length of polling interval
   */
  final case class Error(
    error: Error.Kind,
    errorDescription: Option[String],
    errorUri: Option[String],
    interval: Option[Duration]
  ) extends AccessTokenResponse

  object Error {

    /**
     * Kind of error that can appear in access token response.
     *
     * @see
     *   https://datatracker.ietf.org/doc/html/rfc6749#section-5.2
     * @see
     *   https://datatracker.ietf.org/doc/html/rfc8628#section-3.5
     */
    sealed trait Kind extends Serializable with Product

    object Kind {
      case object InvalidRequest           extends Kind
      case object InvalidClient            extends Kind
      case object InvalidGrant             extends Kind
      case object UnauthorizedClient       extends Kind
      case object UnsupportedGrantType     extends Kind
      case object InvalidScope             extends Kind
      case object AuthorizationPending     extends Kind
      case object SlowDown                 extends Kind
      case object AccessDenied             extends Kind
      case object ExpiredToken             extends Kind
      final case class Other(code: String) extends Kind

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
   * JSON decoder for [[AccessTokenResponse]].
   */
  val accessTokenResponseJsonDecoder: JsonDecoder[AccessTokenResponse] =
    errorJsonDecoder <> accessTokenJsonDecoder
}
