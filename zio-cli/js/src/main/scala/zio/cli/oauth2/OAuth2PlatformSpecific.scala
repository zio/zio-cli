package zio
package cli
package oauth2

object OAuth2PlatformSpecific {
  def validate(
    provider: OAuth2Provider,
    scope: List[String],
    auxiliaryOptions: Options[OAuth2AuxiliaryOptions],
    args: List[String],
    conf: CliConfig
  ): IO[ValidationError, (List[String], OAuth2Token)] = ???
}
