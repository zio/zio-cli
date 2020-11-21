package zio.cli

import java.time._

import zio.test.Assertion._
import zio.test._

object PrimTypeSpec extends DefaultRunnableSpec {

  def spec = suite("PrimTypeTests")(
    suite("Text Suite") {
      testM("validates everything") {
        checkM(Gen.anyString) { i =>
          assertM(PrimType.Text.validate(i, ParserOptions.default))(equalTo(i))
        }
      }
    },
    suite("Decimal Suite") {
      testM("validate returns proper BigDecimal representation") {
        checkM(Gen.bigDecimal(BigDecimal("1.41421356237309504880168"), BigDecimal("50.4"))) { i =>
          assertM(PrimType.Decimal.validate(i.toString(), ParserOptions.default))(equalTo(i))
        }
      }
    },
    suite("Integer Suite") {
      testM("validate returns proper BigInt representation") {
        checkM(anyBigIntString) { i =>
          assertM(PrimType.Integer.validate(i, ParserOptions.default))(equalTo(BigInt(i)))
        }
      }
    },
    suite("Boolean Suite")(
      testM("validate true combinations returns proper Boolean representation") {
        checkM(anyTrueBooleanString) { i =>
          assertM(PrimType.Bool(None).validate(i, ParserOptions.default))(equalTo(true))
        }
      },
      testM("validate false combinations returns proper Boolean representation") {
        checkM(anyFalseBooleanString) { i =>
          assertM(PrimType.Bool(None).validate(i, ParserOptions.default))(equalTo(false))
        }
      }
    ),
    suite("Instant Suite") {
      testM("validate returns proper Instant representation") {
        checkM(Gen.anyInstant) { i =>
          assertM(PrimType.Instant.validate(i.toString, ParserOptions.default))(equalTo(i))
        }
      }
    },
    suite("LocalDateTime Suite") {
      testM("validate returns proper LocalDateTime representation") {
        checkM(anyLocalDateTime) { i =>
          assertM(PrimType.LocalDateTime.validate(i, ParserOptions.default))(equalTo(LocalDateTime.parse(i)))
        }
      }
    },
    suite("LocalDate Suite") {
      testM("validate returns proper LocalDate representation") {
        checkM(anyLocalDate) { i =>
          assertM(PrimType.LocalDate.validate(i, ParserOptions.default))(equalTo(LocalDate.parse(i)))
        }
      }
    },
    suite("LocalTime Suite") {
      testM("validate returns proper LocalTime representation") {
        checkM(anyLocalTime) { i =>
          assertM(PrimType.LocalTime.validate(i, ParserOptions.default))(equalTo(LocalTime.parse(i)))
        }
      }
    },
    suite("MonthDay Suite") {
      testM("validate returns proper MonthDay representation") {
        checkM(anyMonthDay) { i =>
          assertM(PrimType.MonthDay.validate(i, ParserOptions.default))(equalTo(MonthDay.parse(i)))
        }
      }
    },
    suite("OffsetDateTime Suite") {
      testM("validate returns proper OffsetDateTime representation") {
        checkM(Gen.anyOffsetDateTime) { i =>
          assertM(PrimType.OffsetDateTime.validate(i.toString, ParserOptions.default))(equalTo(i))
        }
      }
    },
    suite("OffsetTime Suite") {
      testM("validate returns proper OffsetTime representation") {
        checkM(Gen.anyOffsetDateTime.map(_.toOffsetTime)) { i =>
          assertM(PrimType.OffsetTime.validate(i.toString, ParserOptions.default))(equalTo(i))
        }
      }
    },
    suite("Year Suite") {
      testM("validate returns proper Year representation") {
        checkM(anyYear) { i =>
          assertM(PrimType.Year.validate(i.toString, ParserOptions.default))(equalTo(i))
        }
      }
    },
    suite("YearMonth Suite") {
      testM("validate returns proper YearMonth representation") {
        checkM(anyYearMonth) { i =>
          assertM(PrimType.YearMonth.validate(i.toString, ParserOptions.default))(equalTo(i))
        }
      }
    }
  )

  val anyBigIntString                         = Gen.long(0, Long.MaxValue).map(BigInt(_)).map(_.toString)
  val anyTrueBooleanString: Gen[Any, String]  = Gen.fromIterable(List("true", "TruE", "1", "y", "yes", "yEs", "on"))
  val anyFalseBooleanString: Gen[Any, String] = Gen.fromIterable(List("false", "FAlSE", "0", "n", "no", "off", "OFF"))

  val anyInstant       = Gen.anyInstant.map(_.atZone(ZoneOffset.UTC))
  val anyLocalDateTime = anyInstant.map(_.toLocalDateTime.toString)
  val anyLocalDate     = anyInstant.map(_.toLocalDate.toString)
  val anyLocalTime     = anyInstant.map(_.toLocalTime.toString)
  val anyMonthDay      = anyInstant.map(d => MonthDay.of(d.getMonthValue, d.getDayOfMonth).toString)
  val anyYear          = Gen.int(Year.MIN_VALUE, Year.MAX_VALUE).map(Year.of)
  val anyYearMonth     = anyYear.map(d => YearMonth.of(d.getValue(), 2))

}
