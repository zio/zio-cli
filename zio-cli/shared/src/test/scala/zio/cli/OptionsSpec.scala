package zio.cli

import zio.test.Assertion._
import zio.test._
import zio.cli.HelpDoc.p
import zio.cli.HelpDoc.Span.{error, text}

import java.time.{LocalDate, MonthDay, Year}
import java.time.format.DateTimeFormatter
import zio.test.ZIOSpecDefault

object OptionsSpec extends ZIOSpecDefault {

  val f: Options[String]              = Options.text("firstname").alias("f")
  val l: Options[String]              = Options.text("lastname")
  val a: Options[BigInt]              = Options.integer("age")
  val aOpt: Options[Option[BigInt]]   = Options.integer("age").optional
  val b: Options[Boolean]             = Options.boolean("verbose", true)
  val m: Options[Map[String, String]] = Options.keyValueMap(name = "defs").alias("d")

  val options: Options[(String, String, BigInt)] = f ++ l ++ a

  def spec = suite("Options Suite")(
    test("validate boolean option without value") {
      val r = Options.validate(b, List("--verbose"), CliConfig.default)

      assertZIO(r)(equalTo(List() -> true))
    },
    test("validate boolean option with followup option") {
      val o = Options.boolean("help", true) ++ Options.boolean("v", true)

      for {
        v1 <- Options.validate(o, Nil, CliConfig.default)
        v2 <- Options.validate(o, "--help" :: Nil, CliConfig.default)
        v3 <- Options.validate(o, "--help" :: "-v" :: Nil, CliConfig.default)
      } yield {
        assert(v1)(equalTo(Nil -> (false -> false))) &&
        assert(v2)(equalTo(List.empty[String] -> (true -> false)) ?? "v2") &&
        assert(v3)(equalTo(List.empty[String] -> (true -> true)) ?? "v3")
      }
    },
    test("validate boolean option with negation") {
      val bNegation: Options[Boolean] =
        Options.boolean("verbose", true, "silent", "s").alias("v")
      for {
        v1 <- Options.validate(bNegation, Nil, CliConfig.default)
        v2 <- Options.validate(bNegation, List("--verbose"), CliConfig.default)
        v3 <- Options.validate(bNegation, List("-v"), CliConfig.default)
        v4 <- Options.validate(bNegation, List("--silent"), CliConfig.default)
        v5 <- Options.validate(bNegation, List("-s"), CliConfig.default)
      } yield {
        assert(v1)(equalTo(Nil -> false)) &&
        assert(v2)(equalTo(Nil -> true)) &&
        assert(v3)(equalTo(Nil -> true)) &&
        assert(v4)(equalTo(Nil -> false)) &&
        assert(v5)(equalTo(Nil -> false))
      }
    },
    test("validate text option") {
      val r = Options.validate(f, List("--firstname", "John"), CliConfig.default)
      assertZIO(r)(equalTo(List() -> "John"))
    },
    test("validate text option with alternative format") {
      val r = Options.validate(f, List("--firstname=John"), CliConfig.default)
      assertZIO(r)(equalTo(List() -> "John"))
    },
    test("validate text option with alias") {
      val r = Options.validate(f, List("-f", "John"), CliConfig.default)
      assertZIO(r)(equalTo(List() -> "John"))
    },
    test("validate integer option") {
      val r = Options.validate(a, List("--age", "100"), CliConfig.default)
      assertZIO(r)(equalTo(List() -> BigInt(100)))
    },
    test("validate option and get remainder") {
      val r = Options.validate(f, List("--firstname", "John", "--lastname", "Doe"), CliConfig.default)
      assertZIO(r)(equalTo(List("--lastname", "Doe") -> "John"))
    },
    test("validate option and get remainder with different ordering") {
      val r = Options.validate(f, List("--bar", "baz", "--firstname", "John", "--lastname", "Doe"), CliConfig.default)
      assertZIO(r)(equalTo(List("--bar", "baz", "--lastname", "Doe") -> "John"))
    },
    test("validate when no valid values are passed") {
      val r = Options.validate(f, List("--lastname", "Doe"), CliConfig.default)
      assertZIO(r.either)(isLeft)
    },
    test("validate when option is passed, but not a following value") {
      val r = Options.validate(f, List("--firstname"), CliConfig.default)
      assertZIO(r.either)(isLeft)
    },
    test("validate invalid option value") {
      val intOption = Options.integer("t")
      val v1        = Options.validate(intOption, List("-t", "abc"), CliConfig.default)
      assertZIO(v1.exit)(
        fails(equalTo(ValidationError(ValidationErrorType.InvalidValue, p(text("abc is not a integer.")))))
      )
    },
    test("validate missing option") {
      val intOption = Options.integer("t")
      val v1        = Options.validate(intOption, List(), CliConfig.default)
      assertZIO(v1.exit)(
        fails(equalTo(ValidationError(ValidationErrorType.MissingValue, p(error("Expected to find -t option.")))))
      )
    },
    test("validate invalid option using withDefault") {
      val o = Options.integer("integer").withDefault(BigInt(0))
      val r = Options.validate(o, List("--integer", "abc"), CliConfig.default)
      assertZIO(r.either)(isLeft)
    },
    test("validate collision of boolean option with negation") {
      val bNegation: Options[Boolean] =
        Options.boolean("v", true, "s") // .alias("v")
      val v1 = Options.validate(bNegation, List("-v", "-s"), CliConfig.default)
      assertZIO(v1.either)(isLeft)
    },
    test("validate case sensitive CLI config") {
      val caseSensitiveConfig = CliConfig(true, 2)
      val f: Options[String]  = Options.text("Firstname").alias("F")
      for {
        r1 <- Options.validate(f, List("--Firstname", "John"), caseSensitiveConfig)
        r2 <- Options.validate(f, List("-F", "John"), caseSensitiveConfig)
        _  <- Options.validate(f, List("--firstname", "John"), caseSensitiveConfig).flip
        _  <- Options.validate(f, List("--firstname", "John"), caseSensitiveConfig).flip
      } yield {
        assert(r1)(equalTo(List() -> "John")) &&
        assert(r2)(equalTo(List() -> "John"))
      }
    },
    test("validate options for cons") {
      Options.validate(options, List("--firstname", "John", "--lastname", "Doe", "--age", "100"), CliConfig.default).map {
        case (_, options) =>
          assertTrue(options == (("John", "Doe", BigInt(100))))
      }
    },
    test("validate options for cons with remainder") {
      val r = Options.validate(options, 
        List("--verbose", "true", "--firstname", "John", "--lastname", "Doe", "--age", "100", "--silent", "false"),
        CliConfig.default
      )
      assertZIO(r)(equalTo(List("--verbose", "true", "--silent", "false") -> (("John", "Doe", BigInt(100)))))
    },
    test("validate non supplied optional") {
      val r = Options.validate(aOpt, List(), CliConfig.default)
      assertZIO(r)(equalTo(List() -> None))
    },
    test("validate non supplied optional with remainder") {
      val r = Options.validate(aOpt, List("--bar", "baz"), CliConfig.default)
      assertZIO(r)(equalTo(List("--bar", "baz") -> None))
    },
    test("validate supplied optional") {
      val r = Options.validate(aOpt, List("--age", "20"), CliConfig.default)
      assertZIO(r)(equalTo(List() -> Some(BigInt(20))))
    },
    test("validate supplied optional with remainder") {
      val r = Options.validate(aOpt, List("--firstname", "John", "--age", "20", "--lastname", "Doe"), CliConfig.default)
      assertZIO(r)(equalTo(List("--firstname", "John", "--lastname", "Doe") -> Some(BigInt(20))))
    },
    test("returns a HelpDoc if an option is not an exact match, but is close") {
      val r = Options.validate(f, List("--firstme", "Alice"), CliConfig.default)
      assertZIO(r.either)(
        equalTo(
          Left(
            ValidationError(
              ValidationErrorType.InvalidValue,
              p(error("""The flag "--firstme" is not recognized. Did you mean --firstname?"""))
            )
          )
        )
      )
    },
    test("returns a HelpDoc if an option with a default value is not an exact match, but is close") {
      val r = Options.validate(f.withDefault("Jack"), List("--firstme"), CliConfig.default)
      assertZIO(r.either)(
        equalTo(
          Left(
            ValidationError(
              ValidationErrorType.InvalidValue,
              p(error("""The flag "--firstme" is not recognized. Did you mean --firstname?"""))
            )
          )
        )
      )
    },
    suite("orElse")(
      test("validate orElse on 2 options") {
        val o = Options.text("string").map(Left(_)) | Options.integer("integer").map(Right(_))
        for {
          i <- Options.validate(o, List("--integer", "2"), CliConfig.default)
          s <- Options.validate(o, List("--string", "two"), CliConfig.default)
        } yield {
          assert(i)(equalTo(List() -> Right(BigInt(2)))) &&
          assert(s)(equalTo(List() -> Left("two")))
        }
      },
      test("validate orElse using fold on 2 options") {
        val o = Options.text("string").map(Left(_)) | Options.integer("integer").map(Right(_))
        val output = o.fold(
          (s: String) => s,
          (n: BigInt) => n.toString
        )
        for {
          i <- Options.validate(output, List("--integer", "2"), CliConfig.default)
          s <- Options.validate(output, List("--string", "two"), CliConfig.default)
        } yield {
          assert(i)(equalTo(List() -> "2")) &&
          assert(s)(equalTo(List() -> "two"))
        }
      },
      test("validate orElse using fold on 3 options") {
        val o = Options.text("string").map(s => Left(Left(s))) |
          Options.integer("integer").map(n => Left(Right(n))) |
          Options.decimal("bigdecimal").map(f => Right(f))
        val output = o.fold(
          (s: String) => s,
          (n: BigInt) => n.toString,
          (d: BigDecimal) => d.toString
        )
        for {
          i <- Options.validate(output, List("--integer", "2"), CliConfig.default)
          s <- Options.validate(output, List("--string", "two"), CliConfig.default)
          d <- Options.validate(output, List("--bigdecimal", "3.14"), CliConfig.default)
        } yield {
          assert(i)(equalTo(List() -> "2")) &&
          assert(s)(equalTo(List() -> "two")) &&
          assert(d)(equalTo(List() -> "3.14"))
        }
      },
      test("validate orElse using fold on 4 options") {
        val o = Options.text("string").map(s => Left(Left(Left(s)))) |
          Options.integer("integer").map(n => Left(Left(Right(n)))) |
          Options.decimal("bigdecimal").map(f => Left(Right(f))) |
          Options.localDate("localdate").map(d => Right(d))
        val output = o.fold(
          (s: String) => s,
          (n: BigInt) => n.toString,
          (d: BigDecimal) => d.toString,
          (e: LocalDate) => e.format(DateTimeFormatter.ISO_DATE)
        )
        for {
          i <- Options.validate(output, List("--integer", "2"), CliConfig.default)
          s <- Options.validate(output, List("--string", "two"), CliConfig.default)
          d <- Options.validate(output, List("--bigdecimal", "3.14"), CliConfig.default)
          e <- Options.validate(output, List("--localdate", "2020-01-01"), CliConfig.default)
        } yield {
          assert(i)(equalTo(List() -> "2")) &&
          assert(s)(equalTo(List() -> "two")) &&
          assert(d)(equalTo(List() -> "3.14")) &&
          assert(e)(equalTo(List() -> "2020-01-01"))
        }
      },
      test("validate orElse using fold on 5 options") {
        val o = Options.text("string").map(s => Left(Left(Left(Left(s))))) |
          Options.integer("integer").map(n => Left(Left(Left(Right(n))))) |
          Options.decimal("bigdecimal").map(f => Left(Left(Right(f)))) |
          Options.localDate("localdate").map(d => Left(Right(d))) |
          Options.monthDay("monthday").map(m => Right(m))
        val output = o.fold(
          (s: String) => s,
          (n: BigInt) => n.toString,
          (d: BigDecimal) => d.toString,
          (e: LocalDate) => e.format(DateTimeFormatter.ISO_DATE),
          (f: MonthDay) => f.toString
        )
        for {
          i <- Options.validate(output, List("--integer", "2"), CliConfig.default)
          s <- Options.validate(output, List("--string", "two"), CliConfig.default)
          d <- Options.validate(output, List("--bigdecimal", "3.14"), CliConfig.default)
          e <- Options.validate(output, List("--localdate", "2020-01-01"), CliConfig.default)
          f <- Options.validate(output, List("--monthday", "--01-01"), CliConfig.default)
        } yield {
          assert(i)(equalTo(List() -> "2")) &&
          assert(s)(equalTo(List() -> "two")) &&
          assert(d)(equalTo(List() -> "3.14")) &&
          assert(e)(equalTo(List() -> "2020-01-01")) &&
          assert(f)(equalTo(List() -> "--01-01"))
        }
      },
      test("validate orElse using fold on 6 options") {
        val o = Options.text("string").map(s => Left(Left(Left(Left(Left(s)))))) |
          Options.integer("integer").map(n => Left(Left(Left(Left(Right(n)))))) |
          Options.decimal("bigdecimal").map(f => Left(Left(Left(Right(f))))) |
          Options.localDate("localdate").map(d => Left(Left(Right(d)))) |
          Options.monthDay("monthday").map(m => Left(Right(m))) |
          Options.year("year").map(y => Right(y))
        val output = o.fold(
          (s: String) => s,
          (n: BigInt) => n.toString,
          (d: BigDecimal) => d.toString,
          (e: LocalDate) => e.format(DateTimeFormatter.ISO_DATE),
          (f: MonthDay) => f.toString,
          (g: Year) => g.toString
        )
        for {
          i <- Options.validate(output, List("--integer", "2"), CliConfig.default)
          s <- Options.validate(output, List("--string", "two"), CliConfig.default)
          d <- Options.validate(output, List("--bigdecimal", "3.14"), CliConfig.default)
          e <- Options.validate(output, List("--localdate", "2020-01-01"), CliConfig.default)
          f <- Options.validate(output, List("--monthday", "--01-01"), CliConfig.default)
          g <- Options.validate(output, List("--year", "2020"), CliConfig.default)
        } yield {
          assert(i)(equalTo(List() -> "2")) &&
          assert(s)(equalTo(List() -> "two")) &&
          assert(d)(equalTo(List() -> "3.14")) &&
          assert(e)(equalTo(List() -> "2020-01-01")) &&
          assert(f)(equalTo(List() -> "--01-01")) &&
          assert(g)(equalTo(List() -> "2020"))
        }
      },
      test("test orElse options collision") {
        val o = Options.text("string") | Options.integer("integer")
        val r = Options.validate(o, List("--integer", "2", "--string", "two"), CliConfig.default)
        assertZIO(r.either)(isLeft)
      },
      test("test orElse with no options given") {
        val o = Options.text("string") | Options.integer("integer")
        val r = Options.validate(o, Nil, CliConfig.default)
        assertZIO(r.either)(isLeft)
      },
      test("validate invalid option in OrElse option when using withDefault") {
        val o = (Options.integer("min") | Options.integer("max")).withDefault(BigInt(0))
        val r = Options.validate(o, List("--min", "abc"), CliConfig.default)
        assertZIO(r.either)(isLeft)
      }
    ),
    test("returns a HelpDoc if an option is not an exact match and it's a short option") {
      val r = Options.validate(a, List("--ag", "20"), CliConfig.default)
      assertZIO(r.either)(
        equalTo(Left(ValidationError(ValidationErrorType.MissingValue, p(error("Expected to find --age option.")))))
      )
    },
    suite("property arguments")(
      test("validate missing option") {
        val r = Options.validate(m, List(), CliConfig.default)
        assertZIO(r.exit)(
          fails(equalTo(ValidationError(ValidationErrorType.MissingValue, p(error("Expected to find --defs option.")))))
        )
      },
      test("validate repeated values") {
        val r = Options.validate(m, List("-d", "key1=v1", "-d", "key2=v2", "--verbose"), CliConfig.default)

        assertZIO(r)(equalTo(List("--verbose") -> Map("key1" -> "v1", "key2" -> "v2")))
      },
      test("validate different key/values with alias") {
        val r = Options.validate(m, List("-d", "key1=v1", "key2=v2", "--verbose"), CliConfig.default)

        assertZIO(r)(equalTo(List("--verbose") -> Map("key1" -> "v1", "key2" -> "v2")))
      },
      test("validate different key/values") {
        val r = Options.validate(m, List("--defs", "key1=v1", "key2=v2", "--verbose"), CliConfig.default)

        assertZIO(r)(equalTo(List("--verbose") -> Map("key1" -> "v1", "key2" -> "v2")))
      },
      test(
        "validate should keep non-key-value parameters that follow the key-value pairs (each preceded by alias -d)"
      ) {
        val r = Options.validate(m, 
          List("-d", "key1=val1", "-d", "key2=val2", "-d", "key3=val3", "arg1", "arg2", "--verbose"),
          CliConfig.default
        )

        assertZIO(r)(
          equalTo(List("arg1", "arg2", "--verbose") -> Map("key1" -> "val1", "key2" -> "val2", "key3" -> "val3"))
        )
      },
      test(
        "validate should keep non-key-value parameters that follow the key-value pairs (only the first key/value pair is preceded by alias)"
      ) {
        val r = Options.validate(m, 
          List("-d", "key1=val1", "key2=val2", "key3=val3", "arg1", "arg2", "--verbose"),
          CliConfig.default
        )

        assertZIO(r)(
          equalTo(List("arg1", "arg2", "--verbose") -> Map("key1" -> "val1", "key2" -> "val2", "key3" -> "val3"))
        )
      },
      test(
        "validate should keep non-key-value parameters that follow the key-value pairs (with a 'mixed' style of proceeding -- name or alias)"
      ) {
        val r = Options.validate(m, 
          List("-d", "key1=val1", "key2=val2", "--defs", "key3=val3", "key4=", "arg1", "arg2", "--verbose"),
          CliConfig.default
        )

        assertZIO(r)(
          equalTo(
            List("key4=", "arg1", "arg2", "--verbose") -> Map("key1" -> "val1", "key2" -> "val2", "key3" -> "val3")
          )
        )
      }
    )
  )
}
