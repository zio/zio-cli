package zio.cli.config

/**
 * Merges config file options with CLI arguments.
 *
 * Rules:
 *   1. CLI arguments always override file-based options.
 *   2. Higher-priority files (closer to cwd) override lower-priority files.
 *   3. No duplicate keys in the final output.
 */
object ConfigMerger {

  def merge(
    fileOptions: List[ConfigOption],
    cliArgs: List[String]
  ): List[String] = mergeWithDiagnostics(fileOptions, cliArgs)._1

  def mergeWithDiagnostics(
    fileOptions: List[ConfigOption],
    cliArgs: List[String]
  ): (List[String], ConfigDiagnostics) = {

    // Identify keys provided in CLI arguments
    val cliKeysMap = cliArgs.flatMap { arg =>
      val eqIdx = arg.indexOf('=')
      if (eqIdx > 0 && arg.startsWith("-")) Some(arg.substring(0, eqIdx) -> arg)
      else if (arg.startsWith("-"))          Some(arg -> arg)
      else                                   None
    }.toMap

    val cliKeys = cliKeysMap.keySet

    // Group file options by key
    val groupedOptions = fileOptions.groupBy(_.key)

    var resolvedOptionsList = List.empty[ConfigOption]
    var conflictsList       = List.empty[ConfigConflict]
    var cliOverridesList    = List.empty[String]

    groupedOptions.foreach { case (key, options) =>
      val sortedOpts        = options.sortBy(_.priority)
      val isOverriddenByCli = cliKeys.contains(key)

      if (isOverriddenByCli) {
        cliOverridesList = key :: cliOverridesList
        if (sortedOpts.nonEmpty)
          conflictsList = ConfigConflict(key, sortedOpts, None, cliOverride = true) :: conflictsList
      } else {
        val winner = sortedOpts.last
        resolvedOptionsList = winner :: resolvedOptionsList

        if (sortedOpts.size > 1)
          conflictsList = ConfigConflict(
            key,
            sortedOpts.init,
            Some(winner),
            cliOverride = false
          ) :: conflictsList
      }
    }

    val diagnostics = ConfigDiagnostics(
      resolvedOptionsList.reverse,
      conflictsList.reverse,
      cliOverridesList.reverse
    )

    val fileArgs = resolvedOptionsList.reverse.flatMap { opt =>
      opt.value match {
        case Some(v) => List(s"${opt.key}=$v")
        case None    => List(opt.key)
      }
    }

    (fileArgs ++ cliArgs, diagnostics)
  }
}
