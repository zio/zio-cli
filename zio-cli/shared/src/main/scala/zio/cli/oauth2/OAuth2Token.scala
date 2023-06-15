package zio
package cli
package oauth2

/**
 * Access point to OAuth2 access token.
 *
 * Normal lifecycle of the access token is handled automatically when calling [[accessToken]], i. e. when token has
 * normally expired, it will be refreshed. However, if the token is invalidated in any other way, it will not be
 * detected and therefore the token will not be refreshed automatically. In such case, [[refreshTokenNow]] can be
 * called.
 */
trait OAuth2Token {

  /**
   * Returns current access token. This may call authorization server for refreshing the token, if it has already
   * expired.
   */
  def accessToken: Task[AccessToken]

  /**
   * Refresh access token, either by using refresh token (if available) or by reauthorziing.
   */
  def refreshTokenNow: Task[Unit]
}
