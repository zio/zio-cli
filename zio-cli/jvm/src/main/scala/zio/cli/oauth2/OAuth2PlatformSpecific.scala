package zio
package cli
package oauth2

import zio.cli.HelpDoc.p

object OAuth2PlatformSpecific {
  def validate(
    provider: OAuth2Provider,
    scope: List[String],
    auxiliaryOptions: Options[OAuth2AuxiliaryOptions],
    args: List[String],
    conf: CliConfig
  ): IO[ValidationError, (List[String], OAuth2Token)] =
    auxiliaryOptions.validate(args, conf).flatMap { case (args, aux) =>
      new OAuth2(provider, aux.file, scope).loadOrAuthorize
        .mapError(ex => ValidationError(ValidationErrorType.InvalidValue, p(ex.getMessage)))
        .map(token => (args, token))
    }
}
