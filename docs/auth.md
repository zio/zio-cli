---
id: auth
title: "Authorization with OAuth2"
sidebar_label: "OAuth2"
---

**ZIO CLI** can perform interactions requiring OAuth2. OAuth2 is added to a CLI App as an `Options[OAuth2Token]`. The token can be stored and the user can specify the path, so it is not necessary to repeat authentication. We can create it as with other options:
```scala mdoc:silent
import zio.cli._
import zio.cli.oauth2.OAuth2Provider

val clientId = "clientId"

val provider: OAuth2Provider = OAuth2Provider.Github(clientId)
val scope: List[String] = List("repo")
val oauth2 = Options.oauth2(provider, scope)
```

## Construction
Currently the supported OAuth2 providers are GitHub, Google and Facebook.
```scala mdoc:silent

val clientSecret = "clientSecret"
val appId = "appId"
val clientToken = "clientToken"

val githubOAuth = Options.oauth2(OAuth2Provider.Github(clientId), List("repo"))

val googleOAuth = Options.oauth2(OAuth2Provider.Google(clientId, clientSecret), Nil)

val facebookOAuth = Options.oauth2(OAuth2Provider.Facebook(appId, clientToken), Nil)

```

## Full example
This example shows how to integrate OAuth2 in a ZIO `CliApp`. We are going to make a CLI App that interacts with Github and has two commands:
- Save text in a path (This does not need OAuth2)
- Upload a file to GitHub (This needs OAuth2)

```scala mdoc:silent
import zio.Console.printLine
import zio.cli.HelpDoc.Span.text

object OurCli extends ZIOCliDefault {
  // Construct options
  val options1 = Options.file("path") ++ Options.text("text")
  val options2 = Options.file("path") ++ githubOAuth
  val options = options1 orElseEither options2

  // Construct command
  val command = Command("sample", options)

  // Construct CLI
  val cliApp = CliApp.make(
    name = "OAuth2 Example",
    version = "0.0.1",
    summary = text("Example of CliApp with OAuth2"),
    command = command) {
    // Implement logic of CliApp
      case Left((path, text)) => printLine("Save text in a path")
      case Right((path, oauth2)) => printLine("Upload a file to GitHub")
    }
}
```


