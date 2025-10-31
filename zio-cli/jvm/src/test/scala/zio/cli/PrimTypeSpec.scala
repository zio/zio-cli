package zio.cli

import java.nio.file.{Paths => JPaths}
import java.time._

import zio.cli.files.FileSystem
import zio.{IO, UIO, ZIO}
import zio.test.Assertion._
import zio.test._

import java.time.ZoneOffset
import java.time.MonthDay
import java.time.Year
import java.time.YearMonth
import zio.Random
import zio.test.{Gen, ZIOSpecDefault}

object PrimTypeSpec extends ZIOSpecDefault {

  def spec = suite("PrimTypeTests")(
    suite("Text Suite") {
      test("validates everything") {
        check(Gen.string) { i =>
          assertZIO(PrimType.Text.validate(i))(equalTo(i))
        }
      }
    },
    suite("Enumeration Suite")(
      test("validate return proper value if one of the cases") {
        check(anyPairs) { case ((selectedName, selectedValue), pairs) =>
          assertZIO(PrimType.Enumeration(pairs: _*).validate(selectedName))(equalTo(selectedValue))
        }
      },
      test("validate return error if NOT one of the cases") {
        check(anyPairs) { case (v @ (selectedName, _), pairs) =>
          assertZIO(
            PrimType.Enumeration(pairs.filterNot(_ == v): _*).validate(selectedName).either
          )(isLeft(startsWithString(s"Expected one of the following cases:")))
        }
      }
    ),
    suite("Boolean Suite")(
      test("validate true combinations returns proper Boolean representation") {
        check(anyTrueBooleanString) { i =>
          assertZIO(PrimType.Bool(None).validate(i))(equalTo(true))
        }
      },
      test("validate false combinations returns proper Boolean representation") {
        check(anyFalseBooleanString) { i =>
          assertZIO(PrimType.Bool(None).validate(i))(equalTo(false))
        }
      },
      test("validate rejects improper Boolean representation") {
        assertZIO(PrimType.Bool(None).validate("bad").either)(
          isLeft(equalTo("bad cannot be recognized as valid boolean."))
        )
      },
      test("validate uses default value if value is not provided") {
        check(anyBoolean) { b =>
          assertZIO(PrimType.Bool(Some(b)).validate(None, CliConfig.default))((equalTo(b)))
        }
      }
    ),
    suite("Path Suite")(
      test("validate returns proper directory path") {
        assertZIO(
          PrimType
            .Path(PathType.Directory, shouldExist = Exists.Yes, mockFileSystem(pathIsDirectory = true))
            .validate("path")
        )(equalTo(JPaths.get("path")))
      },
      test("validate returns proper directory path if both allowed") {
        assertZIO(
          PrimType
            .Path(PathType.Either, shouldExist = Exists.Yes, mockFileSystem(pathIsDirectory = true))
            .validate("path")
        )(equalTo(JPaths.get("path")))
      },
      test("validate returns error if path targets file but directory was expected") {
        assertZIO(
          PrimType
            .Path(PathType.Directory, shouldExist = Exists.Yes, mockFileSystem(pathIsRegularFile = true))
            .validate("path")
            .either
        )(isLeft(equalTo("Expected path 'path' to be a directory.")))
      },
      test("validate returns proper file path") {
        assertZIO(
          PrimType
            .Path(PathType.File, shouldExist = Exists.Yes, mockFileSystem(pathIsRegularFile = true))
            .validate("path")
        )(equalTo(JPaths.get("path")))
      },
      test("validate returns proper file path if both allowed") {
        assertZIO(
          PrimType
            .Path(PathType.Either, shouldExist = Exists.Yes, mockFileSystem(pathIsRegularFile = true))
            .validate("path")
        )(equalTo(JPaths.get("path")))
      },
      test("validate returns error if path targets directory but file was expected") {
        assertZIO(
          PrimType
            .Path(PathType.File, shouldExist = Exists.Yes, mockFileSystem(pathIsDirectory = true))
            .validate("path")
            .either
        )(isLeft(equalTo("Expected path 'path' to be a regular file.")))
      },
      test("validate returns error if file doesn't exits but must exists") {
        assertZIO(
          PrimType
            .Path(PathType.Either, shouldExist = Exists.Yes, mockFileSystem(pathExists = false))
            .validate("path")
            .either
        )(isLeft(equalTo("Path 'path' must exist.")))
      },
      test("validate returns error if file does exits but must not exists") {
        assertZIO(
          PrimType.Path(PathType.Either, shouldExist = Exists.No, mockFileSystem()).validate("path").either
        )(isLeft(equalTo("Path 'path' must not exist.")))
      }
    ),
    simplePrimTypeSuite(
      PrimType.Decimal,
      Gen.bigDecimal(BigDecimal("1.41421356237309504880168"), BigDecimal("50.4")),
      "BigDecimal"
    ),
    simplePrimTypeSuite(PrimType.Duration, anyDuration, "Duration"),
    simplePrimTypeSuite(PrimType.Integer, anyBigInt, "Integer"),
    simplePrimTypeSuite(PrimType.Instant, Gen.instant, "Instant"),
    simplePrimTypeSuite(PrimType.LocalDateTime, anyLocalDateTime, "LocalDateTime"),
    simplePrimTypeSuite(PrimType.LocalDate, anyLocalDate, "LocalDate"),
    simplePrimTypeSuite(PrimType.LocalTime, anyLocalTime, "LocalTime"),
    simplePrimTypeSuite(PrimType.MonthDay, anyMonthDay, "MonthDay"),
    simplePrimTypeSuite(PrimType.OffsetDateTime, Gen.offsetDateTime, "OffsetDateTime"),
    simplePrimTypeSuite(PrimType.OffsetTime, anyOffsetTime, "OffsetTime"),
    simplePrimTypeSuite(PrimType.Year, anyYear, "Year"),
    simplePrimTypeSuite(PrimType.YearMonth, anyYearMonth, "YearMonth"),
    simplePrimTypeSuite(PrimType.ZoneOffset, anyZoneOffset, "ZoneOffset"),
    simplePrimTypeSuite(PrimType.ZoneId, anyZoneId, "ZoneId"),
    simplePrimTypeSuite(PrimType.ZonedDateTime, anyZonedDateTime, "ZonedDateTime"),
    simplePrimTypeSuite(PrimType.Period, anyPeriod, "Period")
  )

  def simplePrimTypeSuite[G, P[G] <: PrimType[G]](primType: P[G], gen: Gen[Any, G], primeTypeName: String) =
    suite(s"$primeTypeName Suite")(
      test(s"validate returns proper $primeTypeName representation") {
        check(gen) { i =>
          assertZIO(primType.validate(i.toString))(equalTo(i))
        }
      },
      test(s"validate rejects improper $primeTypeName representation") {
        assertZIO(primType.validate("bad").either)(isLeft(equalTo(s"bad is not a ${primType.typeName}.")))
      }
    )

  def randomizeCharCases(s: String): UIO[String] =
    ZIO.foreach(s.toList)(c => Random.nextBoolean.map(b => if (b) c.toUpper else c.toLower)).map(_.mkString)

  def mockFileSystem(
    correctPath: Boolean = true,
    pathExists: Boolean = true,
    pathIsDirectory: Boolean = false,
    pathIsRegularFile: Boolean = false
  ) = new FileSystem {
    override def parsePath(path: String): IO[String, JPath] =
      if (correctPath) ZIO.succeed(JPaths.get(path)) else ZIO.fail(s"'$path' is not a recognized path.")

    override def exists(path: JPath): UIO[Boolean] = ZIO.succeed(pathExists)

    override def isDirectory(path: JPath): UIO[Boolean] = ZIO.succeed(pathIsDirectory)

    override def isRegularFile(path: JPath): UIO[Boolean] = ZIO.succeed(pathIsRegularFile)
  }

  val anyTrueBooleanString: Gen[Any, String] =
    Gen.fromIterable(List("true", "1", "y", "yes", "on")).mapZIO(randomizeCharCases)
  val anyFalseBooleanString: Gen[Any, String] =
    Gen.fromIterable(List("false", "0", "n", "no", "off")).mapZIO(randomizeCharCases)

  val anyBigInt: Gen[Any, BigInt]   = Gen.long(0, Long.MaxValue).map(BigInt(_))
  val anyBoolean: Gen[Any, Boolean] =
    Gen.fromIterable(List(true, false))
  val anyDuration = Gen.finiteDuration
  val anyInstant  = Gen.instant.map(_.atZone(ZoneOffset.UTC))
  val anyPeriod   = for {
    first  <- Gen.localDateTime.map(_.toLocalDate)
    second <- Gen.localDateTime.map(_.toLocalDate)
  } yield if (first isBefore second) Period.between(first, second) else Period.between(second, first)
  val anyPairs = for {
    uniquePairs <- Gen.listOfBounded(2, 100)(Gen.alphaNumericString.zip(Gen.long)).map(distinctBy(_)(_._1).toList)
    selected    <- Gen.fromIterable(uniquePairs)
  } yield (selected, uniquePairs)
  val anyOffsetTime    = Gen.offsetDateTime.map(_.toOffsetTime)
  val anyZoneOffset    = Gen.offsetDateTime.map(_.getOffset)
  val anyZonedDateTime = Gen.offsetDateTime.map(_.toZonedDateTime)
  val anyZoneId        = anyZonedDateTime.map(_.getZone)
  val anyLocalDateTime = anyInstant.map(_.toLocalDateTime)
  val anyLocalDate     = anyInstant.map(_.toLocalDate)
  val anyLocalTime     = anyInstant.map(_.toLocalTime)
  val anyMonthDay      = anyInstant.map(d => MonthDay.of(d.getMonthValue, d.getDayOfMonth))
  val anyYear          = Gen.int(Year.MIN_VALUE, Year.MAX_VALUE).map(Year.of)
  val anyYearMonth     = anyYear.map(d => YearMonth.of(d.getValue(), 2))

  def distinctBy[A, B](in: Iterable[A])(f: A => B): Iterable[A] =
    in.groupBy(f).values.map(_.head)

}
