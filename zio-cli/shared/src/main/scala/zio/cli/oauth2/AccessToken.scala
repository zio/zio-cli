package zio
package cli
package oauth2

import java.time.OffsetDateTime

import zio.json.{DeriveJsonCodec, JsonCodec}

final case class AccessToken(
  accessToken: String,
  tokenType: TokenType,
  expiresAt: Option[OffsetDateTime],
  refreshToken: Option[String],
  scope: List[String]
)

object AccessToken {
  implicit val stateJsonCodec: JsonCodec[AccessToken] = DeriveJsonCodec.gen[AccessToken]
}
