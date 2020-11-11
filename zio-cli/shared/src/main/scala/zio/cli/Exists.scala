package zio.cli

sealed trait Exists
object Exists {
  case object Yes    extends Exists
  case object No     extends Exists
  case object Either extends Exists
}
