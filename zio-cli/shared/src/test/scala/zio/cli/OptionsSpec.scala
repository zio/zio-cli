package zio.cli

import zio.test.Assertion._
import zio.test._

object OptionsSpec extends DefaultRunnableSpec {

  val f: Options[String]            = Options.text("firstname")
  val l: Options[String]            = Options.text("lastname")
  val a: Options[BigInt]            = Options.integer("age")
  val aOpt: Options[Option[BigInt]] = Options.integer("age").optional

  val options = f :: l :: a

  def spec = suite("Options Suite")(
    testM("validate text option") {
      val r = f.validate(List("--firstname", "John"), ParserOptions.default)
      assertM(r)(equalTo(List() -> "John"))
    },
    testM("validate integer option") {
      val r = a.validate(List("--age", "100"), ParserOptions.default)
      assertM(r)(equalTo(List() -> BigInt(100)))
    },
    testM("validate option and get remainder") {
      val r = f.validate(List("--firstname", "John", "--lastname", "Doe"), ParserOptions.default)
      assertM(r)(equalTo(List("--lastname", "Doe") -> "John"))
    },
    
    testM("validate option and get remainder with different ordering") {
      val r = f.validate(List("--bar", "baz", "--firstname", "John", "--lastname", "Doe"), ParserOptions.default)
      assertM(r)(equalTo(List("--bar", "baz", "--lastname", "Doe") -> "John"))
    },

    testM("validate when no valid values are passed") {
      val r = f.validate(List("--lastname", "Doe"), ParserOptions.default)
      assertM(r.either)(isLeft)
    },
    testM("validate when option is passed, but not a following value") {
      val r        = f.validate(List("--firstname"), ParserOptions.default)
      assertM(r.either)(isLeft)
    },
    testM("validate options for cons") {
      val r = options.validate(List("--firstname", "John", "--lastname", "Doe", "--age", "100"), ParserOptions.default)
      assertM(r)(equalTo(List() -> ("John" -> ("Doe" -> BigInt(100)))))
    },
    testM("validate options for cons with remainder") {
      val r = options.validate(List("--verbose", "true", "--firstname", "John", "--lastname", "Doe", "--age", "100", "--silent", "false"), ParserOptions.default)
      assertM(r)(equalTo(List("--verbose", "true", "--silent", "false") -> ("John" -> ("Doe" -> BigInt(100)))))
    },
    testM("validate non supplied optional") {
      val r = aOpt.validate(List(), ParserOptions.default)
      assertM(r)(equalTo(List() -> None))
    },
    testM("validate non supplied optional with remainder") {
      val r = aOpt.validate(List("--bar", "baz"), ParserOptions.default)
      assertM(r)(equalTo(List("--bar", "baz") -> None))
    },
    testM("validate supplied optional") {
      val r = aOpt.validate(List("--age", "20"), ParserOptions.default)
      assertM(r)(equalTo(List() -> Some(BigInt(20))))
    },
    testM("validate supplied optional with remainder") {
      val r = aOpt.validate(List("--firstname", "John", "--age", "20", "--lastname", "Doe"), ParserOptions.default)
      assertM(r)(equalTo(List("--firstname", "John", "--lastname", "Doe") -> Some(BigInt(20))))
    },
    testM("test requires") {
      val r = f.requires(l).validate(List("--firstname", "John"), ParserOptions.default)
      assertM(r.either)(isLeft)
    },
    testM("test requires not") {
      val r = l.requiresNot(f).validate(List("--firstname", "John"), ParserOptions.default)
      assertM(r.either)(isLeft)
    }
  )
}
