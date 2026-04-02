package zio.cli.config

import zio._

private[cli] trait ConfigFileResolverPlatformSpecific {
  def resolveAndParse(commandName: String): Task[List[ConfigOption]] = {
    val _ = commandName
    ZIO.succeed(Nil)
  }
}
