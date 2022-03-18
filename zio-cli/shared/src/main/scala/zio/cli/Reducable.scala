package zio.cli

trait Reducable[-A, -B] {
  type Out
  def fromTuple2(t: (A, B)): Out
}

object Reducable extends ReducableLowPriority1 {

  type Aux[A, B, C] = Reducable[A, B] { type Out = C }

  implicit def ReducableLeftIdentity[A]: Reducable.Aux[Unit, A, A] = new Reducable[Unit, A] {
    override type Out = A
    override def fromTuple2(t: (Unit, A)): A = t._2
  }
}

trait ReducableLowPriority1 extends ReducableLowPriority2 {
  implicit def ReducableRightIdentity[A]: Reducable.Aux[A, Unit, A] = new Reducable[A, Unit] {
    override type Out = A
    override def fromTuple2(t: (A, Unit)): A = t._1
  }
}

trait ReducableLowPriority2 {
  implicit def tuple[A, B]: Reducable.Aux[A, B, (A, B)] = new Reducable[A, B] {
    override type Out = (A, B)
    override def fromTuple2(t: (A, B)): (A, B) = t
  }
}
