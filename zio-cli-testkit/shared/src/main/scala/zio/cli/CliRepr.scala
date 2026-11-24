package zio.cli.testkit

/**
 * A pair of an input and its expected result: `value` is what is given to the code under test and `repr` is the
 * representation to compare its result with. The assertions in [[CliAssertion]] and [[GenerationAssertion]] take a
 * `Gen` of these pairs, with the list of command-line arguments as `value` and the expected value, `Assertion` or ZIO
 * effect as `repr`.
 *
 * {{{
 * val pair: CliRepr[List[String], String]  = CliRepr(List("greet", "--name", "Ann"), "Ann")
 * val lengths: CliRepr[Int, String]        = pair.map(_.length)
 * val upper: CliRepr[List[String], String] = pair.map2(_.toUpperCase)
 * }}}
 *
 * @param value
 *   the input
 * @param repr
 *   the expected result for `value`
 */
final case class CliRepr[+A, +R](value: A, repr: R) {

  /** Returns a pair with `f` applied to `value` and the same `repr`. */
  def map[B](f: (A => B)) = CliRepr[B, R](f(value), repr)

  /** Returns a pair with the same `value` and `f` applied to `repr`. */
  def map2[B](f: (R => B)) = CliRepr[A, B](value, f(repr))

}
