package zio
package cli

import zio.cli.oauth2._

import java.nio.file.{Path => JPath, Paths => JPaths}

private[cli] trait OptionsPlatformSpecific { self: Options.type =>

  /**
   * Adds into the application OAuth2 authorization with the specified `provider` for permissions
   * defined by `scope`. The resulting [[OAuth2Token]] will provide access token.
   */
  def oauth2(provider: OAuth2Provider, scope: List[String]): Options[OAuth2Token] = {
    val providerSegment = s"${provider.name.toLowerCase()}_${provider.clientIdentifier}"

    val accessTokenFile: Options[JPath] =
      Options
        .file("oauth2-file")
        .withDefault(
          JPaths.get(s"./oauth2_${providerSegment}_access_token.json")
        ) ?? "File in which OAuth2 access token will be stored."

    val auxiliaryOptions: Options[OAuth2AuxiliaryOptions] =
      accessTokenFile.map(OAuth2AuxiliaryOptions.apply)

    OAuth2Options(provider, scope, auxiliaryOptions)
  }

}
