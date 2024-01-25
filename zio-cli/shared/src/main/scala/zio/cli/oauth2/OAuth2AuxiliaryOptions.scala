package zio
package cli
package oauth2

import zio.cli.PathPlatformSpecific.JPath

/**
 * Set of options that can be provided to fine-tune OAuth2 authorization.
 *
 * @param file
 *   path to file where access token will be stored
 */
final case class OAuth2AuxiliaryOptions(file: JPath)
