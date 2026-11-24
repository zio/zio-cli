package zio.cli.testkit

import zio.test.Gen

/**
 * A description of a tree-shaped value that [[Tree#gen]] turns into a generator. A value is built from the `atoms` as
 * leaves, and from the `mappers` (which extend a smaller tree with one more argument) and `operators` (which combine
 * two smaller trees) as inner nodes.
 *
 * {{{
 * import zio.test.Gen
 *
 * val expressions = Tree[String](
 *   atoms = List(Gen.const("x"), Gen.const("y")),
 *   mappers = List(Tree.Mapper[String, Any]((e, n) => e + "*" + n, Gen.const(3))),
 *   operators = List((l: String, r: String) => "(" + l + "+" + r + ")"),
 *   depth = 2
 * )
 *
 * val sample: Gen[Any, String] = expressions.gen
 * // generates values such as "x", "x*3", "(x+y)" and "(x+y)*3"
 * }}}
 *
 * @param atoms
 *   generators of the leaves
 * @param mappers
 *   the nodes with one subtree and one extra argument
 * @param operators
 *   the nodes with two subtrees
 * @param depth
 *   a depth of the tree; [[Tree#gen]] and [[Tree#anyTree]] do not read it
 */
final case class Tree[A](
  atoms: List[Gen[Any, A]],
  mappers: List[Tree.Mapper[A, Any]],
  operators: List[((A, A) => A)],
  depth: Int
) {

  /**
   * A generator of trees whose depth is chosen at random from 0, 1 and 2, built with [[Tree#anyTree]]. It does not use
   * the `depth` field.
   */
  def gen: Gen[Any, A] = for {
    treeDepth <- Gen.bounded(0, 2)(Gen.const(_))
    tree      <- anyTree(treeDepth)
  } yield tree

  /** A generator that picks one of the `atoms` generators at random and generates from it. */
  lazy val anyAtom: Gen[Any, A] = Gen.oneOf(atoms: _*)

  /**
   * A generator of trees of the given depth. At depth 0 or below it generates a leaf from `anyAtom`. At a greater depth
   * it picks at random one of the mappers and operators and applies it to trees of depth `depth - 1`, so the result is
   * always a mapper or operator node and never a bare leaf.
   */
  def anyTree(depth: Int): Gen[Any, A] =
    if (depth <= 0)
      anyAtom
    else {
      val genMappers = mappers.map { case Tree.Mapper(f, anyB) =>
        anyTree(depth - 1).zip(anyB).map { case (a, b) =>
          f(a, b)
        }
      }
      val genOperators = operators.map { case f =>
        anyTree(depth - 1).zip(anyTree(depth - 1)).map { case (a, b) =>
          f(a, b)
        }
      }
      val gens = genMappers ++ genOperators
      Gen.oneOf(gens: _*)
    }
}

/** The node types that a [[Tree]] is built from. */
object Tree {

  /** A pair of a value `a` and the representation `repr` to compare it with. */
  final case class Compare[A, Repr](a: A, repr: Repr)

  /**
   * A tree node with one subtree: `f` combines a generated subtree with a value of type `B` taken from `anyB`.
   * `Tree.mappers` has type `List[Tree.Mapper[A, Any]]`, so give `B` as `Any` there.
   */
  final case class Mapper[A, B](f: ((A, B) => A), anyB: Gen[Any, B])

}
