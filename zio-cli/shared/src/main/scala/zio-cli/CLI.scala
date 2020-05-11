package zio.cli

sealed case class CLI[+A](
  name:    String,
  version: String,
  command: Command[A]
)

object CLI {

  def parse[A](cli: CLI[A], arg: List[String], options: ParserOptions = ParserOptions.default): Either[Vector[String], A] = ???

  def document(cli: CLI[Any]): CliDoc = ???
}