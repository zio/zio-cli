package zio.cli

import zio.test.Assertion._
import zio.test._
import zio.cli.HelpDoc.p
import zio.cli.HelpDoc.Span.error

object OptionsSpec extends DefaultRunnableSpec {

  val f: Options[String]            = Options.text("firstname").alias("f")
  val l: Options[String]            = Options.text("lastname")
  val a: Options[BigInt]            = Options.integer("age")
  val aOpt: Options[Option[BigInt]] = Options.integer("age").optional("N/A")
  val b: Options[Boolean]           = Options.bool("verbose", true)

  val options = f :: l :: a

  def spec = suite("Options Suite")(
    testM("validate boolean option without value") {
      val r = b.validate(List("--verbose"), CliConfig.default)

      assertM(r)(equalTo(List() -> true))
    },
    testM("validate boolean option with followup option") {
      val o = Options.bool("help", true) :: Options.bool("v", true)

      for {
        v1 <- o.validate(Nil, CliConfig.default)
        v2 <- o.validate("--help" :: Nil, CliConfig.default)
        v3 <- o.validate("--help" :: "-v" :: Nil, CliConfig.default)
      } yield {
        assert(v1)(equalTo(Nil                -> (false -> false))) &&
        assert(v2)(equalTo(List.empty[String] -> (true  -> false)) ?? "v2") &&
        assert(v3)(equalTo(List.empty[String] -> (true  -> true)) ?? "v3")
      }
    },
    testM("validate text option 1") {
      val r = f.validate(List("--firstname", "John"), CliConfig.default)
      assertM(r)(equalTo(List() -> "John"))
    },
    testM("validate text option with alias") {
      val r = f.validate(List("-f", "John"), CliConfig.default)
      assertM(r)(equalTo(List() -> "John"))
    },
    testM("validate integer option") {
      val r = a.validate(List("--age", "100"), CliConfig.default)
      assertM(r)(equalTo(List() -> BigInt(100)))
    },
    testM("validate option and get remainder") {
      val r = f.validate(List("--firstname", "John", "--lastname", "Doe"), CliConfig.default)
      assertM(r)(equalTo(List("--lastname", "Doe") -> "John"))
    },
    testM("validate option and get remainder with different ordering") {
      val r = f.validate(List("--bar", "baz", "--firstname", "John", "--lastname", "Doe"), CliConfig.default)
      assertM(r)(equalTo(List("--bar", "baz", "--lastname", "Doe") -> "John"))
    },
    testM("validate when no valid values are passed") {
      val r = f.validate(List("--lastname", "Doe"), CliConfig.default)
      assertM(r.either)(isLeft)
    },
    testM("validate when option is passed, but not a following value") {
      val r = f.validate(List("--firstname"), CliConfig.default)
      assertM(r.either)(isLeft)
    },
    testM("validate options for cons") {
      val r = options.validate(List("--firstname", "John", "--lastname", "Doe", "--age", "100"), CliConfig.default)
      assertM(r)(equalTo(List() -> ("John" -> ("Doe" -> BigInt(100)))))
    },
    testM("validate options for cons with remainder") {
      val r = options.validate(
        List("--verbose", "true", "--firstname", "John", "--lastname", "Doe", "--age", "100", "--silent", "false"),
        CliConfig.default
      )
      assertM(r)(equalTo(List("--verbose", "true", "--silent", "false") -> ("John" -> ("Doe" -> BigInt(100)))))
    },
    testM("validate non supplied optional") {
      val r = aOpt.validate(List(), CliConfig.default)
      assertM(r)(equalTo(List() -> None))
    },
    testM("validate non supplied optional with remainder") {
      val r = aOpt.validate(List("--bar", "baz"), CliConfig.default)
      assertM(r)(equalTo(List("--bar", "baz") -> None))
    },
    testM("validate supplied optional") {
      val r = aOpt.validate(List("--age", "20"), CliConfig.default)
      assertM(r)(equalTo(List() -> Some(BigInt(20))))
    },
    testM("validate supplied optional with remainder") {
      val r = aOpt.validate(List("--firstname", "John", "--age", "20", "--lastname", "Doe"), CliConfig.default)
      assertM(r)(equalTo(List("--firstname", "John", "--lastname", "Doe") -> Some(BigInt(20))))
    },
    testM("returns a HelpDoc if an option is not an exact match, but is close") {
      val r = f.validate(List("--firstme", "Alice"), CliConfig.default)
      assertM(r.either)(
        equalTo(Left(p(error("""The flag "--firstme" is not recognized. Did you mean --firstname?"""))))
      )
    },
    suite("orElse")(
      testM("validate orElse on 2 options") {
        val o = Options.text("string") || Options.integer("integer")
        for {
          i <- o.validate(List("--integer", "2"), CliConfig.default)
          s <- o.validate(List("--string", "two"), CliConfig.default)
        } yield {
          assert(i)(equalTo(List() -> Right(BigInt(2)))) &&
          assert(s)(equalTo(List() -> Left("two")))
        }
      },
      testM("validate orElse using fold on 2 options") {
        val o = Options.text("string") || Options.integer("integer")
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
      testM("validate orElse using fold on 3 options") {
        val o = Options.text("string") || Options.integer("integer") || Options.decimal("bigdecimal")
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
          assert(s)(equalTo(List() -> "two"))
          assert(d)(equalTo(List() -> "3.14"))
        }
      },
      testM("test orElse options collision") {
        val o = Options.text("string") || Options.integer("integer")
        val r = o.validate(List("--integer", "2", "--string", "two"), CliConfig.default)
        assertM(r.either)(isLeft)
      },
      testM("test orElse with no options given") {
        val o = Options.text("string") || Options.integer("integer")
        val r = o.validate(Nil, CliConfig.default)
        assertM(r.either)(isLeft)
      }
    ),
    testM("returns a HelpDoc if an option is not an exact match and it's a short option") {
      val r = a.validate(List("--ag", "20"), CliConfig.default)
      assertM(r.either)(
        equalTo(Left(p(error("Expected to find --age option."))))
      )
    }
  )
}
