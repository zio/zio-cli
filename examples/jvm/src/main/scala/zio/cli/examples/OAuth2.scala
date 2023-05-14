package zio.cli.examples

import zio._
import zio.cli._
import zio.cli.HelpDoc.Span.text
import zio.cli.oauth2.OAuth2Token
import zio.cli.oauth2.OAuth2Provider

object OAuth2 extends ZIOCliDefault {
  case class Cmd(token: OAuth2Token, verbose: Boolean)

  val oauth2: Command[Cmd] =
    Command(
      "oauth2",
      (
        Options.oauth2(OAuth2Provider.Github("client_id"), Nil) ++ Options.boolean("v").alias("verbose")
      ).map { case (t, v) => Cmd(t, v) },
      Args.none
    )

  val cliApp = CliApp.make(
    name = "ZIO CLI OAuth2",
    version = "0.0.1",
    summary = text("Example of OAuth2 authorization using ZIO CLI"),
    command = oauth2
  )(_.token.accessToken.flatMap(at => Console.printLine(at).orDie))

}
