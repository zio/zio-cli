package zio.cli

sealed trait PathType
object PathType {
  case object Either    extends PathType
  case object File      extends PathType
  case object Directory extends PathType
}
