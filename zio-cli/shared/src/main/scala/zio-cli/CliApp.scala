package zio.cli

import zio._

trait CliApp[A] {
  def cli: CLI[A]

  def handler: PartialFunction[A, ZIO[ZEnv, Throwable, Any]]

  final def main(args: Array[String]): Unit = ???
}