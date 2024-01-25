package zio
package cli
package oauth2

import com.github.ghik.silencer.silent

@silent("never used")
private[cli] object OAuth2PlatformSpecific {
  def validate(
    provider: OAuth2Provider,
    scope: List[String],
    auxiliaryOptions: Options[OAuth2AuxiliaryOptions],
    args: Predef.Map[String, List[String]],
    conf: CliConfig
  ): IO[ValidationError, OAuth2Token] =
    ZIO.dieMessage(
      "OAuth2 support is not currently implemented for Scala.js. If you are interested in adding support, please open a pull request at https://github.com/zio/zio-cli."
    )

  def oauth2HelpSection(options: Options[Any]): HelpDoc =
    HelpDoc.empty
}
