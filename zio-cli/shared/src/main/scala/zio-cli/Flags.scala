package zio.cli

/**
 * A `Flag[A]` models a command-line flag that produces a value of type `A`.
 */
sealed trait Flags[+A] { self =>

  def :: [That, A1 >: A](that: Flags[That]): Flags.Cons[That, A1] =
    Flags.Cons(that, self)
}

object Flags {

  final case object Empty extends Flags[Unit]

  final case class Single[+A](name: String, aliases: Vector[String], flagType: Flags.Type[A]) extends Flags[A] {
    import Flags.Type._

    def ? : Flags[Option[A]] = optional

    def alias(name: String): Flags[A] = copy(aliases = aliases :+ name)

    def aliases(names: String*): Flags[A] = copy(aliases = aliases ++ names)

    def map[B](f: A => B): Flags[B] = copy(flagType = Map(flagType, f))

    def optional: Flags[Option[A]] = copy(flagType = Optional(flagType))
  }

  final case class Cons[A, B](left: Flags[A], right: Flags[B]) extends Flags[(A, B)]

  sealed trait Type[+A]
  object Type {
    final case class Toggle(negationName: Option[String], ifPresent: Boolean) extends Type[Boolean]
    final case class Map[A, B](value: Type[A], f: A => B) extends Type[B]
    final case class Optional[A](value: Type[A]) extends Type[Option[A]]
    case object Present extends Type[Unit]
    case object Text extends Type[String]
  }

  /**
   * Creates a boolean flag with the specified name, which, if present, will
   * produce the specified constant boolean value.
   */
  def bool(name: String, ifPresent: Boolean, negationName: Option[String] = None): Single[Boolean] =
    Single(name, Vector.empty, Type.Toggle(negationName, ifPresent))

  def text(name: String): Single[String] =
    Single(name, Vector.empty, Type.Text)

  val empty: Flags[Unit] = Empty
}

