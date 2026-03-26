package zio.cli.config

object ConfigParser {

  def parseLines(lines: List[String], source: String, priority: Int): List[ConfigOption] =
    lines.flatMap(parseLine(_, source, priority))

  private[config] def parseLine(line: String, source: String, priority: Int): Option[ConfigOption] = {
    val trimmed = line.trim

    if (trimmed.isEmpty || trimmed.startsWith("#")) None
    else {
      val tokens = tokenize(trimmed)

      tokens match {
        case Nil =>
          None
        case head :: _ if !head.startsWith("-") =>
          None
        case head :: Nil =>
          Some(ConfigOption(optionKey(head), head :: Nil, source, priority))
        case head :: _ if head.contains("=") =>
          None
        case head :: tail =>
          Some(ConfigOption(optionKey(head), List(head, tail.mkString(" ")), source, priority))
      }
    }
  }

  private[config] def optionKey(token: String): String = {
    val index = token.indexOf('=')
    if (index > 0) token.substring(0, index) else token
  }

  private[config] def tokenize(line: String): List[String] = {
    val builder = scala.collection.mutable.ListBuffer.empty[String]
    val current = new StringBuilder

    var inSingle = false
    var inDouble = false
    var escaped  = false

    def flush(): Unit =
      if (current.nonEmpty) {
        builder += current.toString
        current.clear()
      }

    line.foreach { ch =>
      if (escaped) {
        current.append(ch)
        escaped = false
      } else {
        ch match {
          case '\\' if !inSingle =>
            escaped = true
          case '\'' if !inDouble =>
            inSingle = !inSingle
          case '"' if !inSingle =>
            inDouble = !inDouble
          case c if c.isWhitespace && !inSingle && !inDouble =>
            flush()
          case c =>
            current.append(c)
        }
      }
    }

    if (escaped) current.append('\\')

    flush()
    builder.toList
  }
}
