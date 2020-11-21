package zio.cli

import zio.test.Assertion.equalTo
import zio.test.{ assert, suite, testM, DefaultRunnableSpec }

object ArgsSpec extends DefaultRunnableSpec {

  def spec = suite("Args Suite")(
    testM("validate boolean arguments") {
      val a = Args.bool("boolArg1") ++ Args.bool("boolArg2").repeat

      for {
        v1 <- a.validate("yes" :: Nil, ParserOptions.default)
        v2 <- a.validate("yes" :: "yes" :: Nil, ParserOptions.default)
        v3 <- a.validate("yes" :: "no" :: "yes" :: Nil, ParserOptions.default)
      } yield {
        assert(v1)(equalTo(List.empty[String] -> (true -> List.empty[Boolean])) ?? "v2") &&
        assert(v2)(equalTo(List.empty[String] -> (true -> List(true))) ?? "v3") &&
        assert(v3)(equalTo(List.empty[String] -> (true -> List(false, true))) ?? "v3")
      }
    }
  )
}
