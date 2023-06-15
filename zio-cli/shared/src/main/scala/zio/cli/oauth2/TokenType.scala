package zio
package cli
package oauth2

import zio.json.JsonCodec

/**
 * Access token type informing user about how to use access token.
 *
 * @see
 *   https://datatracker.ietf.org/doc/html/rfc6749#section-7.1
 */
sealed trait TokenType extends Product with Serializable
object TokenType {
  final case object Bearer                  extends TokenType
  final case object Mac                     extends TokenType
  final case class Other(tokenType: String) extends TokenType

  implicit val tokenTypeJsonDecoder: JsonCodec[TokenType] =
    JsonCodec.string.transform(
      {
        case "bearer" => Bearer
        case "mac"    => Mac
        case other    => Other(other)

      },
      {
        case Bearer       => "bearer"
        case Mac          => "mac"
        case Other(other) => other
      }
    )
}
