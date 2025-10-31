package zio.cli

import zio.{IO, ZIO}
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

  def validation[A](options: Options[A], args: List[String], conf: CliConfig): IO[ValidationError, (List[String], A)] =
    Options.validate(options, args, conf).flatMap { case (err, rest, a) =>
      err match {
        case None    => ZIO.succeed((rest, a))
        case Some(e) => ZIO.fail(e)
      }
    }

  def spec = suite("Options Suite")(
    test("validate without ambiguity") {
      val args = List("--firstname", "--lastname", "--lastname", "--firstname")
      val r1   = validation(f ++ l, args, CliConfig.default)
      val r2   = validation(l ++ f, args, CliConfig.default)

      val res1 = (List(), ("--lastname", "--firstname"))
      val res2 = (List(), ("--firstname", "--lastname"))

      assertZIO(r1)(equalTo(res1)) && assertZIO(r2)(equalTo(res2))
    },
    test("not uncluster value") {
      val args = List("--firstname", "-ab")
      val r    = validation(f, args, CliConfig.default)

      val res = (List(), "-ab")

      assertZIO(r)(equalTo(res))
    },
    test("validate boolean option without value") {
      val r = validation(b, List("--verbose"), CliConfig.default)

      assertZIO(r)(equalTo(List() -> true))
    },
    test("validate boolean option with followup option") {
      val o = Options.boolean("help", true) ++ Options.boolean("v", true)

      for {
        v1 <- validation(o, Nil, CliConfig.default)
        v2 <- validation(o, "--help" :: Nil, CliConfig.default)
        v3 <- validation(o, "--help" :: "-v" :: Nil, CliConfig.default)
      } yield {
        assert(v1)(equalTo(Nil -> (false -> false))) &&
        assert(v2)(equalTo(List.empty[String] -> (true -> false)) ?? "v2") &&
        assert(v3)(equalTo(List.empty[String] -> (true -> true)) ?? "v3")
      }
    },
    test("validate boolean option with negation") {
      val bNegation: Options[Boolean] =
        Options.boolean("verbose", "v", true, "silent", "s")
      for {
        v1 <- validation(bNegation, Nil, CliConfig.default).either
        v2 <- validation(bNegation, List("--verbose"), CliConfig.default).either
        v3 <- validation(bNegation, List("-v"), CliConfig.default).either
        v4 <- validation(bNegation, List("--silent"), CliConfig.default).either
        v5 <- validation(bNegation, List("-s"), CliConfig.default).either
      } yield {
        assert(v1)(equalTo(Right(Nil -> false))) &&
        assert(v2)(equalTo(Right(Nil -> true))) &&
        assert(v3)(equalTo(Right(Nil -> true))) &&
        assert(v4)(equalTo(Right(Nil -> false))) &&
        assert(v5)(equalTo(Right(Nil -> false)))
      }
    },
    test("validate text option") {
      val r = validation(f, List("--firstname", "John"), CliConfig.default)
      assertZIO(r)(equalTo(List() -> "John"))
    },
    test("validate text option with alternative format") {
      val r = validation(f, List("--firstname=John"), CliConfig.default)
      assertZIO(r)(equalTo(List() -> "John"))
    },
    test("validate text option with alias") {
      val r = validation(f, List("-f", "John"), CliConfig.default)
      assertZIO(r)(equalTo(List() -> "John"))
    },
    test("validate integer option") {
      val r = validation(a, List("--age", "100"), CliConfig.default)
      assertZIO(r)(equalTo(List() -> BigInt(100)))
    },
    test("validate option and get remainder") {
      val r = validation(f, List("--firstname", "John", "--lastname", "Doe"), CliConfig.default)
      assertZIO(r)(equalTo(List("--lastname", "Doe") -> "John"))
    },
    test("validate when no valid values are passed") {
      val r = validation(f, List("--lastname", "Doe"), CliConfig.default)
      assertZIO(r.either)(isLeft)
    },
    test("validate when option is passed, but not a following value") {
      val r = validation(f, List("--firstname"), CliConfig.default)
      assertZIO(r.either)(isLeft)
    },
    test("validate invalid option value") {
      val intOption = Options.integer("t")
      val v1        = validation(intOption, List("-t", "abc"), CliConfig.default)
      assertZIO(v1.exit)(
        fails(equalTo(ValidationError(ValidationErrorType.InvalidValue, p(text("abc is not a integer.")))))
      )
    },
    test("validate missing option") {
      val intOption = Options.integer("t")
      val v1        = validation(intOption, List(), CliConfig.default)
      assertZIO(v1.exit)(
        fails(equalTo(ValidationError(ValidationErrorType.MissingValue, p(error("Expected to find -t option.")))))
      )
    },
    test("validate invalid option using withDefault") {
      val o = Options.integer("integer").withDefault(BigInt(0))
      val r = validation(o, List("--integer", "abc"), CliConfig.default)
      assertZIO(r.either)(isLeft)
    },
    test("validate collision of boolean option with negation") {
      val bNegation: Options[Boolean] =
        Options.boolean("v", true, "s") // .alias("v")
      val v1 = validation(bNegation, List("-v", "-s"), CliConfig.default)
      assertZIO(v1.either)(isLeft)
    },
    test("validate case sensitive CLI config") {
      val caseSensitiveConfig = CliConfig(true, 2)
      val f: Options[String]  = Options.text("Firstname").alias("F")
      for {
        r1 <- validation(f, List("--Firstname", "John"), caseSensitiveConfig)
        r2 <- validation(f, List("-F", "John"), caseSensitiveConfig)
        _  <- validation(f, List("--firstname", "John"), caseSensitiveConfig).flip
        _  <- validation(f, List("--firstname", "John"), caseSensitiveConfig).flip
      } yield {
        assert(r1)(equalTo(List() -> "John")) &&
        assert(r2)(equalTo(List() -> "John"))
      }
    },
    test("validate options for cons") {
      val r = validation(
        options,
        List("--firstname", "John", "--lastname", "Doe", "--age", "100", "--silent"),
        CliConfig.default
      ).either
      assertZIO(r)(equalTo(Right(List("--silent") -> (("John", "Doe", BigInt(100))))))
    },
    test("validate non supplied optional") {
      val r = validation(aOpt, List(), CliConfig.default)
      assertZIO(r)(equalTo(List() -> None))
    },
    test("validate non supplied optional with remainder") {
      val r = validation(aOpt, List("--bar", "baz"), CliConfig.default)
      assertZIO(r)(equalTo(List("--bar", "baz") -> None))
    },
    test("validate supplied optional") {
      val r = validation(aOpt, List("--age", "20", "--firstname", "John"), CliConfig.default)
      assertZIO(r)(equalTo(List("--firstname", "John") -> Some(BigInt(20))))
    },
    test("returns a HelpDoc if an option is not an exact match, but is close") {
      val r = validation(f, List("--firstme", "Alice"), CliConfig.default)
      assertZIO(r.either)(
        equalTo(
          Left(
            ValidationError(
              ValidationErrorType.CorrectedFlag,
              p(error("""The flag "--firstme" is not recognized. Did you mean --firstname?"""))
            )
          )
        )
      )
    },
    test("returns a HelpDoc if an option with a default value is not an exact match, but is close") {
      val r = validation(f.withDefault("Jack"), List("--firstme"), CliConfig.default)
      assertZIO(r.either)(
        equalTo(
          Left(
            ValidationError(
              ValidationErrorType.CorrectedFlag,
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
          i <- validation(o, List("--integer", "2"), CliConfig.default)
          s <- validation(o, List("--string", "two"), CliConfig.default)
        } yield {
          assert(i)(equalTo(List() -> Right(BigInt(2)))) &&
          assert(s)(equalTo(List() -> Left("two")))
        }
      },
      test("validate orElse using fold on 2 options") {
        val o      = Options.text("string").map(Left(_)) | Options.integer("integer").map(Right(_))
        val output = o.fold(
          (s: String) => s,
          (n: BigInt) => n.toString
        )
        for {
          i <- validation(output, List("--integer", "2"), CliConfig.default)
          s <- validation(output, List("--string", "two"), CliConfig.default)
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
          i <- validation(output, List("--integer", "2"), CliConfig.default)
          s <- validation(output, List("--string", "two"), CliConfig.default)
          d <- validation(output, List("--bigdecimal", "3.14"), CliConfig.default)
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
          i <- validation(output, List("--integer", "2"), CliConfig.default)
          s <- validation(output, List("--string", "two"), CliConfig.default)
          d <- validation(output, List("--bigdecimal", "3.14"), CliConfig.default)
          e <- validation(output, List("--localdate", "2020-01-01"), CliConfig.default)
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
          i <- validation(output, List("--integer", "2"), CliConfig.default)
          s <- validation(output, List("--string", "two"), CliConfig.default)
          d <- validation(output, List("--bigdecimal", "3.14"), CliConfig.default)
          e <- validation(output, List("--localdate", "2020-01-01"), CliConfig.default)
          f <- validation(output, List("--monthday", "--01-01"), CliConfig.default)
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
          i <- validation(output, List("--integer", "2"), CliConfig.default)
          s <- validation(output, List("--string", "two"), CliConfig.default)
          d <- validation(output, List("--bigdecimal", "3.14"), CliConfig.default)
          e <- validation(output, List("--localdate", "2020-01-01"), CliConfig.default)
          f <- validation(output, List("--monthday", "--01-01"), CliConfig.default)
          g <- validation(output, List("--year", "2020"), CliConfig.default)
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
        val r = validation(o, List("--integer", "2", "--string", "two"), CliConfig.default)
        assertZIO(r.either)(isLeft)
      },
      test("test orElse with no options given") {
        val o = Options.text("string") | Options.integer("integer")
        val r = validation(o, Nil, CliConfig.default)
        assertZIO(r.either)(isLeft)
      },
      test("validate invalid option in OrElse option when using withDefault") {
        val o = (Options.integer("min") | Options.integer("max")).withDefault(BigInt(0))
        val r = validation(o, List("--min", "abc"), CliConfig.default)
        assertZIO(r.either)(isLeft)
      }
    ),
    test("returns a HelpDoc if an option is not an exact match and it's a short option") {
      val r = validation(a, List("--ag", "20"), CliConfig.default)
      assertZIO(r.either)(
        equalTo(Left(ValidationError(ValidationErrorType.MissingValue, p(error("Expected to find --age option.")))))
      )
    },
    suite("property arguments")(
      test("validate missing option") {
        val r = validation(m, List(), CliConfig.default)
        assertZIO(r.exit)(
          fails(equalTo(ValidationError(ValidationErrorType.MissingValue, p(error("Expected to find --defs option.")))))
        )
      },
      test("validate repeated values") {
        val r = validation(m, List("-d", "key1=v1", "-d", "key2=v2", "--verbose"), CliConfig.default)

        assertZIO(r.either)(equalTo(Right(List("--verbose") -> Map("key1" -> "v1", "key2" -> "v2"))))
      },
      test("validate different key/values with alias") {
        val r = validation(m, List("-d", "key1=v1", "key2=v2", "--verbose"), CliConfig.default)

        assertZIO(r)(equalTo(List("--verbose") -> Map("key1" -> "v1", "key2" -> "v2")))
      },
      test("validate different key/values") {
        val r = validation(m, List("--defs", "key1=v1", "key2=v2", "--verbose"), CliConfig.default)

        assertZIO(r)(equalTo(List("--verbose") -> Map("key1" -> "v1", "key2" -> "v2")))
      },
      test(
        "validate should keep non-key-value parameters that follow the key-value pairs (each preceded by alias -d)"
      ) {
        val r = validation(
          m,
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
        val r = validation(
          m,
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
        val r = validation(
          m,
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
          """--age <integer>
            |  An integer.
            |
            |  This setting is optional.
            |""".stripMargin.trim
      )
    },
    test("Help describes default value if it is not None") {
      assertTrue(
        lOpt.helpDoc.toPlaintext(color = false).trim ==
          """--lastname <text>
            |  A user-defined piece of text.
            |
            |  This setting is optional. Default: 'xyz'.
            |""".stripMargin.trim
      )
    },
    test("Can overwrite the placeholder used in the help string") {
      assertTrue(
        lOpt
          .withPseudoName("NAME")
          .helpDoc
          .toPlaintext(color = false)
          .trim ==
          """--lastname <NAME>
            |  A user-defined piece of text.
            |
            |  This setting is optional. Default: 'xyz'.
            |""".stripMargin.trim,
        aOpt.withPseudoName("age").helpDoc.toPlaintext(color = false).trim ==
          """--age <age>
            |  An integer.
            |
            |  This setting is optional.
            |""".stripMargin.trim
      )
    }
  )
}
