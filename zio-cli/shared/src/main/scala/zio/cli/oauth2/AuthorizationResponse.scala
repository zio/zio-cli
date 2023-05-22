package zio
package cli
package oauth2

import zio.json.{jsonMemberNames, DeriveJsonDecoder, JsonDecoder, SnakeCase}

/**
 * Response to device authorization request providing end-user verification
 * code.
 *
 * @see
 *   https://datatracker.ietf.org/doc/html/rfc8628#section-3.2
 *
 * @param deviceCode
 *   device verification code
 * @param userCode
 *   end-user verification code
 * @param verificationUri
 *   end-user verification URI
 * @param verificationUriComplete
 *   end-user verification URI with `userCode`
 * @param expiresIn
 *   lifetime od `deviceCode` and `userCode`
 * @param interval
 *   polling interval
 */
final case class AuthorizationResponse(
  deviceCode: String,
  userCode: String,
  verificationUri: String,
  verificationUriComplete: Option[String],
  expiresIn: Duration,
  interval: Duration
)

object AuthorizationResponse {

  @jsonMemberNames(SnakeCase)
  private[this] final case class AuthorizationResponseRaw(
    deviceCode: String,
    userCode: String,
    verificationUri: String,
    verificationUriComplete: Option[String],
    expiresIn: Long,
    interval: Option[Long]
  )

  /**
   * JSON decoder for [[AuthorizationResponse]].
   */
  val authorizationResponseJsonDecoder: JsonDecoder[AuthorizationResponse] =
    DeriveJsonDecoder
      .gen[AuthorizationResponseRaw]
      .map(raw =>
        AuthorizationResponse(
          raw.deviceCode,
          raw.userCode,
          raw.verificationUri,
          raw.verificationUriComplete,
          Duration.fromSeconds(raw.expiresIn),
          raw.interval.map(Duration.fromSeconds).getOrElse(5.seconds)
        )
      )
}
