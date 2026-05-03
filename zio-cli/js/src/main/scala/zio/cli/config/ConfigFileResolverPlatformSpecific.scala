package zio.cli.config

import zio._

/**
 * JS implementation of config file resolution — no-op stub. ScalaJS does not support `java.nio.file`, so dotfile config
 * is unavailable.
 */
trait ConfigFileResolverPlatformSpecific {

  def resolveAndParse(commandName: String): Task[List[ConfigOption]] = {
    val _ = commandName
    ZIO.succeed(Nil)
  }
}
