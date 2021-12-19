package zio.cli

import zio.test.Assertion.{equalTo, isSubtype}
import zio.test.assert
import zio.test.ZIOSpecDefault

object ReducableSpec extends ZIOSpecDefault {
  override def spec =
    suite("Reducable Spec")(
      test("Can reduce left side units") {
        assert(
          implicitly[Reducable[Unit, Int]].fromTuple2(((), 1))
        )(isSubtype[Int](equalTo(1)))
      },
      test("Can reduce right side units") {
        assert(
          implicitly[Reducable[Int, Unit]].fromTuple2((1, ()))
        )(isSubtype[Int](equalTo(1)))
      },
      test("Can reduce tupled units") {
        assert(
          implicitly[Reducable[Unit, Unit]].fromTuple2(((), ()))
        )(isSubtype[Unit](equalTo(())))
      },
      test("Leaves non unit tuples intact") {
        assert(
          implicitly[Reducable[Int, String]].fromTuple2((1, "Howdy"))
        )(isSubtype[(Int, String)](equalTo((1, "Howdy"))))
      }
    )
}
