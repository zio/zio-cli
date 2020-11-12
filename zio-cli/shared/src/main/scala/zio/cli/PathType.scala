package zio.cli

/**
 * Describes whether the command-line application wants a path to be a file or a directory.
 */
sealed trait PathType
object PathType {
  case object Either    extends PathType
  case object File      extends PathType
  case object Directory extends PathType
}
