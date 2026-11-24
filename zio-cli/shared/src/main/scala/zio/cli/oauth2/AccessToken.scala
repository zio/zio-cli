package zio
package cli
package oauth2

import java.time.OffsetDateTime

import zio.json.{DeriveJsonCodec, JsonCodec}

/**
 * An access token as `Options.oauth2` keeps it: the token, its type, the time it expires, the refresh token and the
 * granted scope. `OAuth2Token#accessToken` returns it, and it is saved to the `--oauth2-file` file with
 * `stateJsonCodec`.
 *
 * @param expiresAt
 *   the time the token expires, computed from the `expires_in` of the response when the token was received; `None` when
 *   the response had no `expires_in`
 * @param refreshToken
 *   the token to send in a refresh request; after a refresh whose response has no refresh token, the previous one is
 *   kept
 */
final case class AccessToken(
  accessToken: String,
  tokenType: TokenType,
  expiresAt: Option[OffsetDateTime],
  refreshToken: Option[String],
  scope: List[String]
)

/**
 * Companion of `AccessToken`, holding its JSON codec.
 */
object AccessToken {

  /**
   * Encodes and decodes an `AccessToken` as a JSON object with the field names of the case class (`accessToken`,
   * `tokenType`, `expiresAt`, `refreshToken`, `scope`). `expiresAt` is an ISO-8601 date-time, fields that are `None`
   * are left out when encoding, and `scope` is required when decoding.
   *
   * {{{
   * import java.time.OffsetDateTime
   * import zio.json._
   *
   * val token = AccessToken("gho_16C7e42F", TokenType.Bearer, Some(OffsetDateTime.parse("2026-11-24T10:00:00Z")), None, List("repo"))
   * val json  = token.toJson
   * // {"accessToken":"gho_16C7e42F","tokenType":"bearer","expiresAt":"2026-11-24T10:00:00Z","scope":["repo"]}
   * json.fromJson[AccessToken] == Right(token)
   * // true
   * }}}
   */
  implicit val stateJsonCodec: JsonCodec[AccessToken] = DeriveJsonCodec.gen[AccessToken]
}
