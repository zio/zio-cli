package zio
package cli
package oauth2

import zio.cli.HelpDoc.{h1, p}
import zio.cli.Options._

private[cli] object OAuth2PlatformSpecific {
  def validate(
    provider: OAuth2Provider,
    scope: List[String],
    auxiliaryOptions: Options[OAuth2AuxiliaryOptions],
    args: Predef.Map[String, List[String]],
    conf: CliConfig
  ): IO[ValidationError, OAuth2Token] =
    auxiliaryOptions.validate(args, conf).flatMap { case aux =>
      new OAuth2(provider, aux.file, scope).loadOrAuthorize
        .mapError(ex => ValidationError(ValidationErrorType.InvalidValue, p(ex.getMessage)))
    }

  def findProvider(opt: Options[Any]): Option[OAuth2Provider] =
    opt match {
      case Both(left, right)             => findProvider(left).orElse(findProvider(right))
      case Options.Map(value, _)         => findProvider(value)
      case KeyValueMap(_)                => None
      case Empty                         => None
      case Options.OrElse(left, right)   => findProvider(left).orElse(findProvider(right))
      case Options.Single(_, _, _, _)    => None
      case OAuth2Options(provider, _, _) => Some(provider)
      case WithDefault(options, _)       => findProvider(options)
    }

  def oauth2HelpSection(options: Options[Any]): HelpDoc =
    findProvider(options).fold(HelpDoc.empty) { provider =>
      h1("3rd party authorization") + HelpDoc.p(
        s"""|This application requires 3rd party authorization (using OAuth2 protocol)
            |provided by ${provider.name}. When the application is launched for the first time,
            |instructions to perform the authorization will be displayed. Subsequent launches
            |do not require any action unless the access has been revoked.
            |
            |Behavior of 3rd party authorization can be modified by options starting with '--oauth2-'.""".stripMargin
      )
    }

}
