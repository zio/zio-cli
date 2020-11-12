package zio.cli

/**
 * Describes whether the command-line application wants a file/directory to exist or not exist.
 */
sealed trait Exists
object Exists {
  case object Yes    extends Exists
  case object No     extends Exists
  case object Either extends Exists
}
