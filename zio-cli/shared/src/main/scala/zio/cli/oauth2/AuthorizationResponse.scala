package zio
package cli
package oauth2

import zio.json.{jsonMemberNames, DeriveJsonDecoder, JsonDecoder, SnakeCase}

/**
 * The response to a device authorization request: the device code the client polls with, and the code and URI the user
 * needs to approve the request in a browser.
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
 *   lifetime of `deviceCode` and `userCode`
 * @param interval
 *   time to wait between two token requests
 */
final case class AuthorizationResponse(
  deviceCode: String,
  userCode: String,
  verificationUri: String,
  verificationUriComplete: Option[String],
  expiresIn: Duration,
  interval: Duration
)

/**
 * Companion of `AuthorizationResponse`, holding the decoder of the standard JSON response.
 */
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
   * Decodes the JSON response of RFC 8628 section 3.2. `device_code`, `user_code`, `verification_uri` and `expires_in`
   * are required, and `verification_uri_complete` and `interval` are optional. `expires_in` and `interval` are read as
   * seconds, and a missing `interval` gives 5 seconds.
   *
   * {{{
   * AuthorizationResponse.authorizationResponseJsonDecoder.decodeJson(
   *   """{"device_code":"3584d83530557fdd","user_code":"WDJB-MJHT","verification_uri":"https://example.com/device","expires_in":900}"""
   * )
   * // Right(AuthorizationResponse(3584d83530557fdd,WDJB-MJHT,https://example.com/device,None,PT15M,PT5S))
   * }}}
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
