package zio.cli

sealed trait OptionFormat
object OptionFormat {
  case object Snake extends OptionFormat
  case object Camel extends OptionFormat
  case object Kebab extends OptionFormat

  val all: Set[OptionFormat] = Set(Snake, Camel, Kebab)
}
