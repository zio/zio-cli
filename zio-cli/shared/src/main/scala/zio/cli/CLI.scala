package zio.cli

import zio._

final case class CLI[-R, +E](
  name: String,
  version: String,
  command: Command[R, E],
  options: ParserOptions = ParserOptions.default
) {
  def run(args: List[String]): ZIO[R, Nothing, Int] = ???

  def helpDoc: HelpDoc = ???
}
