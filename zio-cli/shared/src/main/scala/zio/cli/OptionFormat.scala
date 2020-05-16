package zio.cli

/**
 * An `OptionFormat` represents the format of options. There are three common
 * formats: camel case, snake case, and kebab case.
 *
 * For command-line applications, the most common option format is `Kebab`.
 */
sealed trait OptionFormat
object OptionFormat {
  case object Snake extends OptionFormat
  case object Camel extends OptionFormat
  case object Kebab extends OptionFormat

  val all: Set[OptionFormat] = Set(Snake, Camel, Kebab)
}
