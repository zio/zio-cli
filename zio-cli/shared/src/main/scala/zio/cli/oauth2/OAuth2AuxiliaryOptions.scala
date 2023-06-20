package zio
package cli
package oauth2

import java.nio.file.Path

/**
 * Set of options that can be provided to fine-tune OAuth2 authorization.
 *
 * @param file
 *   path to file where access token will be stored
 */
final case class OAuth2AuxiliaryOptions(file: Path)
