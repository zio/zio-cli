package zio
package cli
package oauth2

import zio.cli.PathPlatformSpecific.JPath

/**
 * The values of the extra command-line options that `Options.oauth2` adds to fine-tune OAuth2 authorization. Their
 * names start with `--oauth2-`.
 *
 * @param file
 *   path of the file the access token is loaded from and saved to, given with `--oauth2-file`
 */
final case class OAuth2AuxiliaryOptions(file: JPath)
