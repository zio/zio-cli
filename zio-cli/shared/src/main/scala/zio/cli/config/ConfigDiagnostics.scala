package zio.cli.config

import zio._

final case class ConfigOption(
  key: String,
  arguments: List[String],
  source: String,
  priority: Int
)

final case class ConfigConflict(
  key: String,
  overridden: List[ConfigOption],
  winner: Option[ConfigOption],
  cliOverride: Boolean
)

final case class ConfigDiagnostics(
  resolvedOptions: List[ConfigOption],
  conflicts: List[ConfigConflict],
  cliOverrides: List[String]
) {
  def nonEmpty: Boolean = resolvedOptions.nonEmpty || conflicts.nonEmpty || cliOverrides.nonEmpty
}

object ConfigDiagnostics {

  def printDiagnostics(diagnostics: ConfigDiagnostics): UIO[Unit] = {
    val printResolved = ZIO.foreachDiscard(diagnostics.resolvedOptions) { option =>
      val rendered = option.arguments.mkString(" ")
      Console.printLine(s"config: $rendered (from ${option.source})").ignore
    }

    val printOverrides = ZIO.foreachDiscard(diagnostics.cliOverrides.distinct.sorted) { key =>
      Console.printLine(s"config: CLI overrides $key").ignore
    }

    val printConflicts = ZIO.foreachDiscard(diagnostics.conflicts.filterNot(_.cliOverride)) { conflict =>
      val winnerSource = conflict.winner.map(_.source).getOrElse("unknown")
      Console.printLine(s"config: resolved ${conflict.key} from $winnerSource").ignore
    }

    (printResolved *> printOverrides *> printConflicts).when(diagnostics.nonEmpty).unit
  }
}
