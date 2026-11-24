package zio.cli.completion

import zio._
import zio.cli.PathType
import zio.cli.PrimType

import java.time.{ZoneId => JZoneId}
import scala.jdk.CollectionConverters._

/**
 * Lists the tab completions for the value of an option or an argument, from its [[zio.cli.PrimType]].
 */
object PrimTypeCompletion {

  /**
   * Returns the completions that start with `prefix` for a value of `primType`. `PrimType.Bool` gives `"true "` and
   * `"false "`, `PrimType.Enumeration` gives its case names, and `PrimType.ZoneId` gives the ids of
   * `java.time.ZoneId.getAvailableZoneIds`, each followed by a space. `PrimType.Path` gives the file and directory
   * names that `compgen` lists (only directories for `PathType.Directory`), and a failure of `compgen` becomes a
   * defect. The other types, such as `PrimType.Text`, `PrimType.Integer` and the date and time types, give an empty
   * set. The prefix is compared with case.
   */
  def firstTokens(primType: PrimType[Any], prefix: String, compgen: Compgen): UIO[Set[String]] =
    primType match {
      case PrimType.Bool(_) =>
        ZIO.succeed(Set("true", "false").filter(_.startsWith(prefix))).map(appendSpaces)
      case PrimType.Decimal =>
        ZIO.succeed(Set.empty)
      case PrimType.Duration =>
        ZIO.succeed(Set.empty)
      case PrimType.Enumeration(cases @ _*) =>
        ZIO
          .succeed(cases.collect {
            case (name, _) if name.startsWith(prefix) => name
          }.toSet)
          .map(appendSpaces)
      case PrimType.Instant =>
        ZIO.succeed(Set.empty)
      case PrimType.Integer =>
        ZIO.succeed(Set.empty)
      case PrimType.LocalDate =>
        ZIO.succeed(Set.empty)
      case PrimType.LocalDateTime =>
        ZIO.succeed(Set.empty)
      case PrimType.LocalTime =>
        ZIO.succeed(Set.empty)
      case PrimType.MonthDay =>
        ZIO.succeed(Set.empty)
      case PrimType.OffsetDateTime =>
        ZIO.succeed(Set.empty)
      case PrimType.OffsetTime =>
        ZIO.succeed(Set.empty)
      case PrimType.Path(PathType.Either | PathType.File, _, _) =>
        compgen.completeFileNames(prefix).map(_.toSet).orDie
      case PrimType.Path(PathType.Directory, _, _) =>
        compgen.completeDirectoryNames(prefix).map(_.toSet).orDie
      case PrimType.Period =>
        ZIO.succeed(Set.empty)
      case PrimType.Text =>
        ZIO.succeed(Set.empty)
      case PrimType.Year =>
        ZIO.succeed(Set.empty)
      case PrimType.YearMonth =>
        ZIO.succeed(Set.empty)
      case PrimType.ZoneId =>
        ZIO.succeed(JZoneId.getAvailableZoneIds().iterator.asScala.filter(_.startsWith(prefix)).toSet).map(appendSpaces)
      case PrimType.ZoneOffset =>
        ZIO.succeed(Set.empty)
      case PrimType.ZonedDateTime =>
        ZIO.succeed(Set.empty)
    }

  /**
   * Returns `tokens` with a space added to the end of each one (`Set("true")` gives `Set("true ")`).
   */
  def appendSpaces(tokens: Set[String]): Set[String] = tokens.map(_ + " ")
}
