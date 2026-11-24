package zio
package cli
package oauth2

import zio.json.JsonCodec

/**
 * The `token_type` of an access token, which tells the client how to use the token.
 *
 * @see
 *   https://datatracker.ietf.org/doc/html/rfc6749#section-7.1
 */
sealed trait TokenType extends Product with Serializable

/**
 * The token types and their JSON codec.
 */
object TokenType {

  /**
   * The `bearer` token type. It is decoded from the string `"bearer"` only, in lower case.
   */
  final case object Bearer extends TokenType

  /**
   * The `mac` token type. It is decoded from the string `"mac"` only, in lower case.
   */
  final case object Mac extends TokenType

  /**
   * A token type other than `"bearer"` and `"mac"`, kept as the string received. A different case counts as another
   * type: `"Bearer"` decodes to `Other("Bearer")`.
   */
  final case class Other(tokenType: String) extends TokenType

  /**
   * Encodes and decodes a `TokenType` as a JSON string. Decoding is case-sensitive: `"bearer"` gives `Bearer`, `"mac"`
   * gives `Mac`, and any other string, such as `"Bearer"` or `"DPoP"`, gives `Other` with that string. Encoding writes
   * `bearer`, `mac` or the string inside `Other`, so `Other("bearer")` decodes back as `Bearer`.
   */
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
