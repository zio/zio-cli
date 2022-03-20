package zio.cli.completion

import zio._
import zio.cli.PathType
import zio.cli.PrimType

import java.time.{ZoneId => JZoneId}
import scala.jdk.CollectionConverters._

object PrimTypeCompletion {
  def firstTokens(primType: PrimType[Any], prefix: String, compgen: Compgen): UIO[Set[String]] =
    primType match {
      case PrimType.Bool(_) =>
        ZIO.succeed(Set("true", "false").filter(_.startsWith(prefix))).map(appendSpaces)
      case PrimType.Decimal =>
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
      case PrimType.Path(PathType.Either | PathType.File, shouldExist, fileSystem) =>
        compgen.completeFileNames(prefix).map(_.toSet).orDie
      case PrimType.Path(PathType.Directory, shouldExist, fileSystem) =>
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

  def appendSpaces(tokens: Set[String]): Set[String] = tokens.map(_ + " ")
}
