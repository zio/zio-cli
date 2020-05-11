package zio.cli

sealed trait CliDoc {
  def toPlaintext(columnWidth: Int = 100, color: Boolean = true): String = ???
  def toJson: String = ???
  def toHtml: String = ???
}

object CliDoc {

}
