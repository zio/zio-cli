package zio.cli.config

object ConfigMerger {

  def merge(fileOptions: List[ConfigOption], cliArgs: List[String]): List[String] =
    mergeWithDiagnostics(fileOptions, cliArgs)._1

  def mergeWithDiagnostics(
    fileOptions: List[ConfigOption],
    cliArgs: List[String]
  ): (List[String], ConfigDiagnostics) = {
    val cliKeys = cliArgs.flatMap(arg => if (arg.startsWith("-")) Some(ConfigParser.optionKey(arg)) else None).toSet

    val groupedByKey = fileOptions.groupBy(_.key)

    val resolvedByKey = groupedByKey.collect {
      case (key, options) if !cliKeys.contains(key) =>
        key -> pickWinner(options)
    }

    val resolvedOptions =
      fileOptions.filter(option => resolvedByKey.get(option.key).contains(option))

    val conflicts = groupedByKey.toList.sortBy(_._1).flatMap { case (key, options) =>
      val sortedByPriority = options.sortBy(_.priority)
      if (cliKeys.contains(key)) {
        Some(ConfigConflict(key, sortedByPriority, None, cliOverride = true))
      } else if (sortedByPriority.size > 1) {
        val winner = pickWinner(sortedByPriority)
        Some(ConfigConflict(key, sortedByPriority.filterNot(_ == winner), Some(winner), cliOverride = false))
      } else {
        None
      }
    }

    val mergedArgs = resolvedOptions.flatMap(_.arguments) ++ cliArgs

    (
      mergedArgs,
      ConfigDiagnostics(
        resolvedOptions = resolvedOptions,
        conflicts = conflicts,
        cliOverrides = cliKeys.toList
      )
    )
  }

  private def pickWinner(options: List[ConfigOption]): ConfigOption =
    options.reduceLeft { (current, next) =>
      if (next.priority >= current.priority) next else current
    }
}
