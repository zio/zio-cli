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
  val lOpt: Options[String]           = Options.text("lastname").withDefault("xyz")
  val a: Options[BigInt]              = Options.integer("age")
  val aOpt: Options[Option[BigInt]]   = Options.integer("age").optional
  val b: Options[Boolean]             = Options.boolean("verbose", true)
  val m: Options[Map[String, String]] = Options.keyValueMap(name = "defs").alias("d")

  val options: Options[(String, String, BigInt)] = f ++ l ++ a

  def spec = suite("Options Suite")(
    test("validate boolean option without value") {
      val r = b.validate(List("--verbose"), CliConfig.default)

      assertZIO(r)(equalTo(List() -> true))
    },
    test("validate boolean option with followup option") {
      val o = Options.boolean("help", true) ++ Options.boolean("v", true)

      for {
        v1 <- o.validate(Nil, CliConfig.default)
        v2 <- o.validate("--help" :: Nil, CliConfig.default)
        v3 <- o.validate("--help" :: "-v" :: Nil, CliConfig.default)
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
        v1 <- bNegation.validate(Nil, CliConfig.default)
        v2 <- bNegation.validate(List("--verbose"), CliConfig.default)
        v3 <- bNegation.validate(List("-v"), CliConfig.default)
        v4 <- bNegation.validate(List("--silent"), CliConfig.default)
        v5 <- bNegation.validate(List("-s"), CliConfig.default)
        _  <- bNegation.validate(List("--silent", "--verbose"), CliConfig.default).flip // colliding options
        _  <- bNegation.validate(List("-s", "-v"), CliConfig.default).flip              // colliding options
      } yield {
        assert(v1)(equalTo(Nil -> false)) &&
        assert(v2)(equalTo(Nil -> true)) &&
        assert(v3)(equalTo(Nil -> true)) &&
        assert(v4)(equalTo(Nil -> false)) &&
        assert(v5)(equalTo(Nil -> false))
      }
    },
    test("validate text option") {
      val r = f.validate(List("--firstname", "John"), CliConfig.default)
      assertZIO(r)(equalTo(List() -> "John"))
    },
    test("validate text option with alternative format") {
      val r = f.validate(List("--firstname=John"), CliConfig.default)
      assertZIO(r)(equalTo(List() -> "John"))
    },
    test("validate text option with alias") {
      val r = f.validate(List("-f", "John"), CliConfig.default)
      assertZIO(r)(equalTo(List() -> "John"))
    },
    test("validate integer option") {
      val r = a.validate(List("--age", "100"), CliConfig.default)
      assertZIO(r)(equalTo(List() -> BigInt(100)))
    },
    test("validate option and get remainder") {
      val r = f.validate(List("--firstname", "John", "--lastname", "Doe"), CliConfig.default)
      assertZIO(r)(equalTo(List("--lastname", "Doe") -> "John"))
    },
    test("validate option and get remainder with different ordering") {
      val r = f.validate(List("--bar", "baz", "--firstname", "John", "--lastname", "Doe"), CliConfig.default)
      assertZIO(r)(equalTo(List("--bar", "baz", "--lastname", "Doe") -> "John"))
    },
    test("validate when no valid values are passed") {
      val r = f.validate(List("--lastname", "Doe"), CliConfig.default)
      assertZIO(r.either)(isLeft)
    },
    test("validate when option is passed, but not a following value") {
      val r = f.validate(List("--firstname"), CliConfig.default)
      assertZIO(r.either)(isLeft)
    },
    test("validate invalid option value") {
      val intOption = Options.integer("t")
      val v1        = intOption.validate(List("-t", "abc"), CliConfig.default)
      assertZIO(v1.exit)(
        fails(equalTo(ValidationError(ValidationErrorType.InvalidValue, p(text("abc is not a integer.")))))
      )
    },
    test("validate missing option") {
      val intOption = Options.integer("t")
      val v1        = intOption.validate(List(), CliConfig.default)
      assertZIO(v1.exit)(
        fails(equalTo(ValidationError(ValidationErrorType.MissingValue, p(error("Expected to find -t option.")))))
      )
    },
    test("validate invalid option using withDefault") {
      val o = Options.integer("integer").withDefault(BigInt(0))
      val r = o.validate(List("--integer", "abc"), CliConfig.default)
      assertZIO(r.either)(isLeft)
    },
    test("validate collision of boolean option with negation") {
      val bNegation: Options[Boolean] =
        Options.boolean("v", true, "s") // .alias("v")
      val v1 = bNegation.validate(List("-v", "-s"), CliConfig.default)
      assertZIO(v1.either)(isLeft)
    },
    test("validate case sensitive CLI config") {
      val caseSensitiveConfig = CliConfig(true, 2)
      val f: Options[String]  = Options.text("Firstname").alias("F")
      for {
        r1 <- f.validate(List("--Firstname", "John"), caseSensitiveConfig)
        r2 <- f.validate(List("-F", "John"), caseSensitiveConfig)
        _  <- f.validate(List("--firstname", "John"), caseSensitiveConfig).flip
        _  <- f.validate(List("--firstname", "John"), caseSensitiveConfig).flip
      } yield {
        assert(r1)(equalTo(List() -> "John")) &&
        assert(r2)(equalTo(List() -> "John"))
      }
    },
    test("validate options for cons") {
      options.validate(List("--firstname", "John", "--lastname", "Doe", "--age", "100"), CliConfig.default).map {
        case (_, options) =>
          assertTrue(options == (("John", "Doe", BigInt(100))))
      }
    },
    test("validate options for cons with remainder") {
      val r = options.validate(
        List("--verbose", "true", "--firstname", "John", "--lastname", "Doe", "--age", "100", "--silent", "false"),
        CliConfig.default
      )
      assertZIO(r)(equalTo(List("--verbose", "true", "--silent", "false") -> (("John", "Doe", BigInt(100)))))
    },
    test("validate non supplied optional") {
      val r = aOpt.validate(List(), CliConfig.default)
      assertZIO(r)(equalTo(List() -> None))
    },
    test("validate non supplied optional with remainder") {
      val r = aOpt.validate(List("--bar", "baz"), CliConfig.default)
      assertZIO(r)(equalTo(List("--bar", "baz") -> None))
    },
    test("validate supplied optional") {
      val r = aOpt.validate(List("--age", "20"), CliConfig.default)
      assertZIO(r)(equalTo(List() -> Some(BigInt(20))))
    },
    test("validate supplied optional with remainder") {
      val r = aOpt.validate(List("--firstname", "John", "--age", "20", "--lastname", "Doe"), CliConfig.default)
      assertZIO(r)(equalTo(List("--firstname", "John", "--lastname", "Doe") -> Some(BigInt(20))))
    },
    test("returns a HelpDoc if an option is not an exact match, but is close") {
      val r = f.validate(List("--firstme", "Alice"), CliConfig.default)
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
      val r = f.withDefault("Jack").validate(List("--firstme"), CliConfig.default)
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
          i <- o.validate(List("--integer", "2"), CliConfig.default)
          s <- o.validate(List("--string", "two"), CliConfig.default)
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
          i <- output.validate(List("--integer", "2"), CliConfig.default)
          s <- output.validate(List("--string", "two"), CliConfig.default)
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
          i <- output.validate(List("--integer", "2"), CliConfig.default)
          s <- output.validate(List("--string", "two"), CliConfig.default)
          d <- output.validate(List("--bigdecimal", "3.14"), CliConfig.default)
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
          i <- output.validate(List("--integer", "2"), CliConfig.default)
          s <- output.validate(List("--string", "two"), CliConfig.default)
          d <- output.validate(List("--bigdecimal", "3.14"), CliConfig.default)
          e <- output.validate(List("--localdate", "2020-01-01"), CliConfig.default)
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
          i <- output.validate(List("--integer", "2"), CliConfig.default)
          s <- output.validate(List("--string", "two"), CliConfig.default)
          d <- output.validate(List("--bigdecimal", "3.14"), CliConfig.default)
          e <- output.validate(List("--localdate", "2020-01-01"), CliConfig.default)
          f <- output.validate(List("--monthday", "--01-01"), CliConfig.default)
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
          i <- output.validate(List("--integer", "2"), CliConfig.default)
          s <- output.validate(List("--string", "two"), CliConfig.default)
          d <- output.validate(List("--bigdecimal", "3.14"), CliConfig.default)
          e <- output.validate(List("--localdate", "2020-01-01"), CliConfig.default)
          f <- output.validate(List("--monthday", "--01-01"), CliConfig.default)
          g <- output.validate(List("--year", "2020"), CliConfig.default)
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
        val r = o.validate(List("--integer", "2", "--string", "two"), CliConfig.default)
        assertZIO(r.either)(isLeft)
      },
      test("test orElse with no options given") {
        val o = Options.text("string") | Options.integer("integer")
        val r = o.validate(Nil, CliConfig.default)
        assertZIO(r.either)(isLeft)
      },
      test("validate invalid option in OrElse option when using withDefault") {
        val o = (Options.integer("min") | Options.integer("max")).withDefault(BigInt(0))
        val r = o.validate(List("--min", "abc"), CliConfig.default)
        assertZIO(r.either)(isLeft)
      }
    ),
    test("returns a HelpDoc if an option is not an exact match and it's a short option") {
      val r = a.validate(List("--ag", "20"), CliConfig.default)
      assertZIO(r.either)(
        equalTo(Left(ValidationError(ValidationErrorType.MissingValue, p(error("Expected to find --age option.")))))
      )
    },
    suite("property arguments")(
      test("validate missing option") {
        val r = m.validate(List(), CliConfig.default)
        assertZIO(r.exit)(
          fails(equalTo(ValidationError(ValidationErrorType.MissingValue, p(error("Expected to find --defs option.")))))
        )
      },
      test("validate repeated values") {
        val r = m.validate(List("-d", "key1=v1", "-d", "key2=v2", "--verbose"), CliConfig.default)

        assertZIO(r)(equalTo(List("--verbose") -> Map("key1" -> "v1", "key2" -> "v2")))
      },
      test("validate different key/values with alias") {
        val r = m.validate(List("-d", "key1=v1", "key2=v2", "--verbose"), CliConfig.default)

        assertZIO(r)(equalTo(List("--verbose") -> Map("key1" -> "v1", "key2" -> "v2")))
      },
      test("validate different key/values") {
        val r = m.validate(List("--defs", "key1=v1", "key2=v2", "--verbose"), CliConfig.default)

        assertZIO(r)(equalTo(List("--verbose") -> Map("key1" -> "v1", "key2" -> "v2")))
      },
      test(
        "validate should keep non-key-value parameters that follow the key-value pairs (each preceded by alias -d)"
      ) {
        val r = m.validate(
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
        val r = m.validate(
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
        val r = m.validate(
          List("-d", "key1=val1", "key2=val2", "--defs", "key3=val3", "key4=", "arg1", "arg2", "--verbose"),
          CliConfig.default
        )

        assertZIO(r)(
          equalTo(
            List("key4=", "arg1", "arg2", "--verbose") -> Map("key1" -> "val1", "key2" -> "val2", "key3" -> "val3")
          )
        )
      }
    ),
    test("Help describes default value but does not print None as default value") {
      assertTrue(
        aOpt.helpDoc.toPlaintext(color = false).trim ==
          """--age integer
            |  An integer.
            |
            |  This setting is optional.
            |""".stripMargin.trim
      )
    },
    test("Help describes default value if it is not None") {
      assertTrue(
        lOpt.helpDoc.toPlaintext(color = false).trim ==
          """--lastname text
            |  A user-defined piece of text.
            |
            |  This setting is optional. Default: 'xyz'.
            |""".stripMargin.trim
      )
    }
  )
}
