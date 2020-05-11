package zio.cli

/**
 * A `Flag[A]` models a command-line flag that produces a value of type `A`.
 */
sealed case class Flag[+A](name: String, aliases: Vector[String], flagType: Flag.Type[A]) {
  import Flag.Type._

  def ? : Flag[Option[A]] = optional

  def alias(name: String): Flag[A] = copy(aliases = aliases :+ name)

  def aliases(names: String*): Flag[A] = copy(aliases = aliases ++ names)

  def map[B](f: A => B): Flag[B] = copy(flagType = Map(flagType, f))

  def optional: Flag[Option[A]] = copy(flagType = Optional(flagType))
}

object Flag {
  sealed trait Type[+A]
  object Type {
    // Make Toggle on/off a primitive????
    sealed case class Toggle(negationName: Option[String], ifPresent: Boolean) extends Type[Boolean]
    case object Present extends Type[Unit]
    sealed case class Map[A, B](value: Type[A], f: A => B) extends Type[B]
    sealed case class Optional[A](value: Type[A]) extends Type[Option[A]]
  }

  // Map(Optional(Present), f : Unit => Boolean)

  // --terse     (optional)

  /**
   * Creates a boolean flag with the specified name, which, if present, will
   * produce the specified constant boolean value.
   */
  def bool(name: String, ifPresent: Boolean, negationName: Option[String] = None): Flag[Boolean] =
    Flag(name, Vector.empty, Type.Toggle(negationName, ifPresent))

  bool("terse", true)
}

