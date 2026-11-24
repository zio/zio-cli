package zio.cli

/**
 * Combines two parsed values into one and drops a side that is `Unit`. `Command.apply` uses it to merge the value of the options with the value of the arguments, and `Command#subcommands` to merge the parent's value with the child's. `Out` is the combined type.
 *
 * {{{
 * val both: Command[(String, BigInt)] = Command("copy", Options.text("mode"), Args.integer)
 * val argsOnly: Command[BigInt]       = Command("count", Args.integer)
 * val nothing: Command[Unit]          = Command("status")
 * }}}
 */
trait Reducable[-A, -B] {

  /** The type of the combined value. */
  type Out

  /** Combines the two values of `t`. */
  def fromTuple2(t: (A, B)): Out
}

/**
 * The instances, from the highest priority: a `Unit` on the left gives the right value, a `Unit` on the right gives the left value, and otherwise the result is the pair. For `(Unit, Unit)` the result is `()`.
 */
object Reducable extends ReducableLowPriority1 {

  /** A [[Reducable]] whose `Out` is `C`. */
  type Aux[A, B, C] = Reducable[A, B] { type Out = C }

  /** Drops a `Unit` on the left: `((), a)` becomes `a`. */
  implicit def ReducableLeftIdentity[A]: Reducable.Aux[Unit, A, A] = new Reducable[Unit, A] {
    override type Out = A
    override def fromTuple2(t: (Unit, A)): A = t._2
  }
}

/** The second-priority instance of [[Reducable]]. */
trait ReducableLowPriority1 extends ReducableLowPriority2 {

  /** Drops a `Unit` on the right: `(a, ())` becomes `a`. */
  implicit def ReducableRightIdentity[A]: Reducable.Aux[A, Unit, A] = new Reducable[A, Unit] {
    override type Out = A
    override def fromTuple2(t: (A, Unit)): A = t._1
  }
}

/** The lowest-priority instance of [[Reducable]]. */
trait ReducableLowPriority2 {

  /** Keeps both values as a pair; used when neither side is `Unit`. */
  implicit def tuple[A, B]: Reducable.Aux[A, B, (A, B)] = new Reducable[A, B] {
    override type Out = (A, B)
    override def fromTuple2(t: (A, B)): (A, B) = t
  }
}
