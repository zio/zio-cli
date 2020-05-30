package zio.cli

import zio.{ IO, ZIO }

/**
 * A `Command[R, E]` is a model of a command that can be given to a command-
 * line application.
 *
 * Commands may have children, which represent subcommands.
 */
sealed trait Command[-R, +E] { self =>
  type A
  type B
  def action: String
  def options: Options[A]
  def args: Args[B]
  def execute(a: A, b: B): ZIO[R, E, Any]
  def children: List[Command[R, E]]

  def options[A1](options0: Options[A1]): Command.Aux[R, E, (self.A, A1), self.B] = new Command[R, E] {
    override type A = (self.A, A1)
    override type B = self.B

    override def action: String = self.action

    override def options: Options[A] = self.options :: options0

    override def args: Args[B] = self.args

    override def execute(
      a: A,
      b: B
    ): ZIO[R, E, Any] = self.execute(a._1, b)

    override def children: List[Command[R, E]] = self.children
  }

  def args[B1](args0: Args[B1]): Command.Aux[R, E, self.A, (self.B, B1)] = new Command[R, E] {
    override type A = self.A
    override type B = (self.B, B1)

    override def action: String = self.action

    override def options: Options[A] = self.options

    override def args: Args[B] = self.args :: args0

    override def execute(
      a: A,
      b: B
    ): ZIO[R, E, Any] = self.execute(a, b._1)

    override def children: List[Command[R, E]] = self.children
  }

  def execute[R1 <: R, E1 >: E](f: (A, B) => ZIO[R1, E1, Any]): Command.Aux[R1, E1, self.A, self.B] =
    new Command[R1, E1] {
      override type A = self.A
      override type B = self.B

      override def action: String = self.action

      override def options: Options[A] = self.options

      override def args: Args[B] = self.args

      override def execute(
        a: A,
        b: B
      ): ZIO[R1, E1, Any] = self.execute(a, b) *> f(a, b)

      override def children: List[Command[R, E]] = self.children
    }

  def children[R1 <: R, E1 >: E](children0: List[Command[R1, E1]]): Command.Aux[R1, E1, self.A, self.B] =
    new Command[R1, E1] {
      override type A = self.A
      override type B = self.B

      override def action: String = self.action

      override def options: Options[A] = self.options

      override def args: Args[B] = self.args

      override def execute(
        a: A,
        b: B
      ): ZIO[R1, E1, Any] = self.execute(a, b)

      override def children: List[Command[R1, E1]] = self.children ++ children0
    }

  /**
   * Generates the help doc for this command, and any subcommands.
   */
  final def helpDoc: HelpDoc.Block = ???

  /**
   * Validates the arguments from the command line, either returning a failure
   * that includes detailed documentation, or returning a tuple that contains
   * both options and arguments, together with remaining (unparsed) arguments
   * from the command-line.
   */
  final def validate(args: List[String], opts: ParserOptions): IO[List[HelpDoc.Block], (List[String], A, B)] =
    children match {
      case Nil =>
        for {
          optsResult         <- options.validate(args, opts)
          (remainingArgs, a) = optsResult
          argsResult         <- this.args.validate(remainingArgs, opts)
          (finalArgs, b)     = argsResult //NOTE: Should we consider having a remainder as a validation failure???
        } yield (finalArgs, a, b)
      case subCommand :: Nil =>
        for {
          optsResult                                            <- options.validate(args, opts)
          (remainingArgs, a)                                    = optsResult
          argsResult                                            <- this.args.validate(remainingArgs, opts)
          (secondArgs, b)                                       = argsResult //NOTE: Should we consider having a remainder as a validation failure???
          subResult: (List[String], subCommand.A, subCommand.B) <- subCommand.validate(secondArgs, opts)
          (subRemaining, sa, sb)                                = subResult
        } yield (subRemaining, (a, sa), (b, sb))
      case subCommands =>
        ???
    }
}

object Command {
  type Aux[-R0, +E0, A0, B0] = Command[R0, E0] {
    type A = A0
    type B = B0
  }

  /**
   * Construct a new command with the specified command name.
   */
  def apply(
    action0: String
  ): Command.Aux[Any, Nothing, Any, Any] = new Command[Any, Nothing] {
    override type A = Any
    override type B = Any

    override def action: String = action0

    override def options: Options[A] = Options.Empty

    override def args: Args[B] = Args.Empty

    override def execute(a: A, b: B): ZIO[Any, Nothing, Any] = ZIO.unit

    override def children: List[Command[Any, Nothing]] = Nil
  }

}
