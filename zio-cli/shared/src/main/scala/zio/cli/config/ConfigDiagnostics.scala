package zio.cli.config

import zio._

/**
 * Diagnostic information about how configuration was resolved.
 */
case class ConfigDiagnostics(
  resolvedOptions: List[ConfigOption],
  conflicts: List[ConfigConflict],
  cliOverrides: List[String]
)

/**
 * Records when the same key appears in multiple config sources.
 */
case class ConfigConflict(
  key: String,
  overridden: List[ConfigOption],
  winner: Option[ConfigOption],
  cliOverride: Boolean
)

object ConfigDiagnostics {

  /**
   * Prints a human-readable summary of where each resolved option came from, which options were overridden by CLI, and
   * detected conflicts.
   */
  def printDiagnostics(diagnostics: ConfigDiagnostics): UIO[Unit] = {
    val header = Console.printLine("\n--- Config Diagnostics ---").ignore

    val printResolved = ZIO.foreachDiscard(diagnostics.resolvedOptions) { opt =>
      val valueStr = opt.value.getOrElse("<flag>")
      Console.printLine(s"  ${opt.key.stripPrefix("-")} = $valueStr   (from ${opt.source})").ignore
    }

    val printCli = ZIO.foreachDiscard(diagnostics.cliOverrides) { key =>
      Console.printLine(s"  ${key.stripPrefix("-")}   (overridden by CLI)").ignore
    }

    val printConflicts = ZIO.foreachDiscard(diagnostics.conflicts) { conflict =>
      Console.printLine(s"  ⚠ Option '${conflict.key.stripPrefix("-")}' overridden:").ignore *>
        ZIO.foreachDiscard(conflict.overridden) { opt =>
          val valueStr = opt.value.getOrElse("<flag>")
          Console.printLine(s"    ${opt.source} → $valueStr").ignore
        } *>
        (if (conflict.cliOverride)
           Console.printLine(s"    (CLI wins)").ignore
         else
           conflict.winner match {
             case Some(w) =>
               val v = w.value.getOrElse("<flag>")
               Console.printLine(s"    Winner: ${w.source} → $v").ignore
             case None => ZIO.unit
           })
    }

    val footer = Console.printLine("--- End Diagnostics ---\n").ignore

    header *> printResolved *> printCli *> printConflicts *> footer
  }
}
