package zio.cli

import zio.ZIO

sealed trait Command[-R, +E] { self =>
  type A
  type B
  def action: String
  def flags: Flags[A]
  def args: Args[B]
  def execute(a: A, b: B): ZIO[R, E, Any]
  def children: List[Command[R, E]]

  def flags[A1](flags0: Flags[A1]): Command.Aux[R, E, (self.A, A1), self.B] = new Command[R, E] {
    override type A = (self.A, A1)
    override type B = self.B

    override def action: String = self.action

    override def flags: Flags[A] = self.flags :: flags0

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

    override def flags: Flags[A] = self.flags

    override def args: Args[B] = self.args :: args0

    override def execute(
      a: A,
      b: B
    ): ZIO[R, E, Any] = self.execute(a, b._1)

    override def children: List[Command[R, E]] = self.children
  }

  def execute[R1 <: R, E1 >: E](f: (A, B) => ZIO[R1, E1, Any]): Command.Aux[R1, E1, self.A, self.B] = new Command[R1, E1] {
    override type A = self.A
    override type B = self.B

    override def action: String = self.action

    override def flags: Flags[A] = self.flags

    override def args: Args[B] = self.args

    override def execute(
      a: A,
      b: B
    ): ZIO[R1, E1, Any] = self.execute(a, b) *> f(a, b)

    override def children: List[Command[R, E]] = self.children
  }

  def children[R1 <: R, E1 >: E](children0: List[Command[R1, E1]]): Command.Aux[R1, E1, self.A, self.B] = new Command[R1, E1] {
    override type A = self.A
    override type B = self.B

    override def action: String = self.action

    override def flags: Flags[A] = self.flags

    override def args: Args[B] = self.args

    override def execute(
      a: A,
      b: B
    ): ZIO[R1, E1, Any] = self.execute(a, b)

    override def children: List[Command[R1, E1]] = self.children ++ children0
  }
}

object Command {

  type Aux[-R0, +E0, A0, B0] = Command[R0, E0] {
    type A = A0
    type B = B0
  }

  def apply(
    action0: String
  ): Command.Aux[Any, Nothing, Any, Any] = new Command[Any, Nothing] {
    override type A = Any
    override type B = Any

    override def action: String = action0

    override def flags: Flags[A] = Flags.Empty

    override def args: Args[B] = Args.Empty

    override def execute(a: A, b: B): ZIO[Any, Nothing, Any] = ZIO.unit

    override def children: List[Command[Any, Nothing]] = Nil
  }

}