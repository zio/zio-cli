package zio.cli

private[cli] object AutoCorrect {
  def levensteinDistance(first: String, second: String, conf: CliConfig): Int =
    (first.length, second.length) match {
      case (0, 0)                  => 0
      case (0, secondLength)       => secondLength
      case (firstLength, 0)        => firstLength
      case (rowCount, columnCount) =>
        val matrix       = Array.ofDim[Int](rowCount + 1, columnCount + 1)
        val normalFirst  = conf.normalizeCase(first)
        val normalSecond = conf.normalizeCase(second)

        (0 to rowCount).foreach(x => matrix(x)(0) = x)
        (0 to columnCount).foreach(x => matrix(0)(x) = x)

        for {
          row <- 1 to rowCount
          col <- 1 to columnCount
        } {
          val cost = if (normalFirst.charAt(row - 1) == normalSecond.charAt(col - 1)) 0 else 1

          matrix(row)(col) = Seq(
            matrix(row)(col - 1) + 1,
            matrix(row - 1)(col) + 1,
            matrix(row - 1)(col - 1) + cost
          ).min
        }

        matrix(rowCount)(columnCount)
    }
}
