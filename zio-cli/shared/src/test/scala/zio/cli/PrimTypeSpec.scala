package zio.cli

import java.nio.file.{Path => JPath, Paths => JPaths}
import java.time._

import zio.cli.files.FileSystem
import zio.{random, IO, UIO, ZIO}
import zio.random.Random
import zio.test.Assertion._
import zio.test._

import java.time.ZoneOffset
import java.time.MonthDay
import java.time.Year
import java.time.YearMonth

object PrimTypeSpec extends DefaultRunnableSpec {

  def spec = suite("PrimTypeTests")(
    suite("Text Suite") {
      testM("validates everything") {
        checkM(Gen.anyString) { i =>
          assertM(PrimType.Text.validate(i))(equalTo(i))
        }
      }
    },
    suite("Enumeration Suite")(
      testM("validate return proper value if one of the cases") {
        checkM(anyPairs) { case ((selectedName, selectedValue), pairs) =>
          assertM(PrimType.Enumeration(pairs: _*).validate(selectedName))(equalTo(selectedValue))
        }
      },
      testM("validate return error if NOT one of the cases") {
        checkM(anyPairs) { case (v @ (selectedName, _), pairs) =>
          assertM(
            PrimType.Enumeration(pairs.filterNot(_ == v): _*).validate(selectedName).either
          )(isLeft(startsWithString(s"Expected one of the following cases:")))
        }
      }
    ),
    suite("Boolean Suite")(
      testM("validate true combinations returns proper Boolean representation") {
        checkM(anyTrueBooleanString) { i =>
          assertM(PrimType.Bool(None).validate(i))(equalTo(true))
        }
      },
      testM("validate false combinations returns proper Boolean representation") {
        checkM(anyFalseBooleanString) { i =>
          assertM(PrimType.Bool(None).validate(i))(equalTo(false))
        }
      },
      testM("validate rejects improper Boolean representation") {
        assertM(PrimType.Bool(None).validate("bad").either)(
          isLeft(equalTo("bad cannot be recognized as valid boolean."))
        )
      },
      testM("validate uses default value if value is not provided") {
        checkM(anyBoolean) { b =>
          assertM(PrimType.Bool(Some(b)).validate(None, CliConfig.default))((equalTo(b)))
        }
      }
    ),
    suite("Path Suite")(
      testM("validate returns proper directory path") {
        assertM(
          PrimType
            .Path(PathType.Directory, shouldExist = Exists.Yes, mockFileSystem(pathIsDirectory = true))
            .validate("path")
        )(equalTo(JPaths.get("path")))
      },
      testM("validate returns proper directory path if both allowed") {
        assertM(
          PrimType
            .Path(PathType.Either, shouldExist = Exists.Yes, mockFileSystem(pathIsDirectory = true))
            .validate("path")
        )(equalTo(JPaths.get("path")))
      },
      testM("validate returns error if path targets file but directory was expected") {
        assertM(
          PrimType
            .Path(PathType.Directory, shouldExist = Exists.Yes, mockFileSystem(pathIsRegularFile = true))
            .validate("path")
            .either
        )(isLeft(equalTo("Expected path 'path' to be a directory.")))
      },
      testM("validate returns proper file path") {
        assertM(
          PrimType
            .Path(PathType.File, shouldExist = Exists.Yes, mockFileSystem(pathIsRegularFile = true))
            .validate("path")
        )(equalTo(JPaths.get("path")))
      },
      testM("validate returns proper file path if both allowed") {
        assertM(
          PrimType
            .Path(PathType.Either, shouldExist = Exists.Yes, mockFileSystem(pathIsRegularFile = true))
            .validate("path")
        )(equalTo(JPaths.get("path")))
      },
      testM("validate returns error if path targets directory but file was expected") {
        assertM(
          PrimType
            .Path(PathType.File, shouldExist = Exists.Yes, mockFileSystem(pathIsDirectory = true))
            .validate("path")
            .either
        )(isLeft(equalTo("Expected path 'path' to be a regular file.")))
      },
      testM("validate returns error if file doesn't exits but must exists") {
        assertM(
          PrimType
            .Path(PathType.Either, shouldExist = Exists.Yes, mockFileSystem(pathExists = false))
            .validate("path")
            .either
        )(isLeft(equalTo("Path 'path' must exist.")))
      },
      testM("validate returns error if file does exits but must not exists") {
        assertM(
          PrimType.Path(PathType.Either, shouldExist = Exists.No, mockFileSystem()).validate("path").either
        )(isLeft(equalTo("Path 'path' must not exist.")))
      }
    ),
    simplePrimTypeSuite(
      PrimType.Decimal,
      Gen.bigDecimal(BigDecimal("1.41421356237309504880168"), BigDecimal("50.4")),
      "BigDecimal"
    ),
    simplePrimTypeSuite(PrimType.Integer, anyBigInt, "Integer"),
    simplePrimTypeSuite(PrimType.Instant, Gen.anyInstant, "Instant"),
    simplePrimTypeSuite(PrimType.LocalDateTime, anyLocalDateTime, "LocalDateTime"),
    simplePrimTypeSuite(PrimType.LocalDate, anyLocalDate, "LocalDate"),
    simplePrimTypeSuite(PrimType.LocalTime, anyLocalTime, "LocalTime"),
    simplePrimTypeSuite(PrimType.MonthDay, anyMonthDay, "MonthDay"),
    simplePrimTypeSuite(PrimType.OffsetDateTime, Gen.anyOffsetDateTime, "OffsetDateTime"),
    simplePrimTypeSuite(PrimType.OffsetTime, anyOffsetTime, "OffsetTime"),
    simplePrimTypeSuite(PrimType.Year, anyYear, "Year"),
    simplePrimTypeSuite(PrimType.YearMonth, anyYearMonth, "YearMonth"),
    simplePrimTypeSuite(PrimType.ZoneOffset, anyZoneOffset, "ZoneOffset"),
    simplePrimTypeSuite(PrimType.ZoneId, anyZoneId, "ZoneId"),
    simplePrimTypeSuite(PrimType.ZonedDateTime, anyZonedDateTime, "ZonedDateTime"),
    simplePrimTypeSuite(PrimType.Period, anyPeriod, "Period")
  )

  def simplePrimTypeSuite[G, P[G] <: PrimType[G]](primType: P[G], gen: Gen[Random, G], primeTypeName: String) =
    suite(s"$primeTypeName Suite")(
      testM(s"validate returns proper $primeTypeName representation") {
        checkM(gen) { i =>
          assertM(primType.validate(i.toString))(equalTo(i))
        }
      },
      testM(s"validate rejects improper $primeTypeName representation") {
        assertM(primType.validate("bad").either)(isLeft(equalTo(s"bad is not a ${primType.typeName}.")))
      }
    )

  def randomizeCharCases(s: String): ZIO[Random, Nothing, String] =
    ZIO.foreach(s.toList)(c => random.nextBoolean.map(b => if (b) c.toUpper else c.toLower)).map(_.mkString)

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

  val anyTrueBooleanString: Gen[Random, String] =
    Gen.fromIterable(List("true", "1", "y", "yes", "on")).mapM(randomizeCharCases)
  val anyFalseBooleanString: Gen[Random, String] =
    Gen.fromIterable(List("false", "0", "n", "no", "off")).mapM(randomizeCharCases)

  val anyBigInt: Gen[Random, BigInt] = Gen.long(0, Long.MaxValue).map(BigInt(_))
  val anyBoolean: Gen[Random, Boolean] =
    Gen.fromIterable(List(true, false))
  val anyInstant = Gen.anyInstant.map(_.atZone(ZoneOffset.UTC))
  val anyPeriod = for {
    first  <- Gen.anyLocalDateTime.map(_.toLocalDate)
    second <- Gen.anyLocalDateTime.map(_.toLocalDate)
  } yield if (first isBefore second) Period.between(first, second) else Period.between(second, first)
  val anyPairs = for {
    uniquePairs <- Gen.listOfBounded(2, 100)(Gen.alphaNumericString.zip(Gen.anyLong)).map(distinctBy(_)(_._1).toList)
    selected    <- Gen.fromIterable(uniquePairs)
  } yield (selected, uniquePairs)
  val anyOffsetTime    = Gen.anyOffsetDateTime.map(_.toOffsetTime)
  val anyZoneOffset    = Gen.anyOffsetDateTime.map(_.getOffset)
  val anyZonedDateTime = Gen.anyOffsetDateTime.map(_.toZonedDateTime)
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
