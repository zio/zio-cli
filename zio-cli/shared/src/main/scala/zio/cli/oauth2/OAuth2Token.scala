package zio
package cli
package oauth2

/**
 * Access point to an OAuth2 access token, the value of an `Options.oauth2` option.
 *
 * [[accessToken]] handles the normal lifecycle of the token: when the token has expired, it is refreshed. If the token
 * is invalidated in any other way, for example revoked by the user, this is not detected and the token is not refreshed
 * automatically. In such a case, call [[refreshTokenNow]].
 */
trait OAuth2Token {

  /**
   * Returns the current access token. If it has expired, it first refreshes it, which calls the authorization server
   * and saves the new token to the token file.
   */
  def accessToken: Task[AccessToken]

  /**
   * Refreshes the access token now, by a refresh request when the token has a refresh token, and otherwise by running
   * the device authorization again, which prints instructions for the user and waits for the approval.
   */
  def refreshTokenNow: Task[Unit]
}
