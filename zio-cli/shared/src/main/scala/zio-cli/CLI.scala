package zio.cli

final case class CLI[-R, +E](
  name:    String,
  version: String,
  command: Command[R, E]
)

object CLI {

//  def parse[A](cli: CLI[A], arg: List[String], options: ParserOptions = ParserOptions.default): Either[Vector[String], A] = ???
//
//  def document(cli: CLI[Any]): CliDoc = ???
}