package zio.config

trait InvariantZip[A, B] {
  type Out

  def combine(a: A, b: B): Out

  def projectLeft(o: Out): A
  def projectRight(o: Out): B
}

object InvariantZip extends InvariantZipLowPriority0 {
  type WithOut[A, B, C] = InvariantZip[A, B] { type Out = C }

  implicit def invariantZipTuple2[A, B, Unit]: InvariantZip.WithOut[(A, B), Unit, (A, B)] = ???

  implicit def invariantZipUnitB[B]: WithOut[Unit, B, B] = new InvariantZip[Unit, B] {
    type Out = B

    override def combine(a: Unit, b: B): Out = b

    override def projectLeft(o: B): Unit = ()

    override def projectRight(o: B): B = o
  }

  implicit def invariantZipAUnit[A]: WithOut[A, Unit, A] = new InvariantZip[A, Unit] {
    type Out = A

    override def combine(a: A, b: Unit): Out = a

    override def projectLeft(o: A): A = o

    override def projectRight(o: A): Unit = ()
  }
}

trait InvariantZipLowPriority0 extends InvariantZipLowPriority1 {
  implicit def invariantZipTuple2[A, B, Z]: InvariantZip.WithOut[(A, B), Z, (A, B, Z)] = ???

  implicit def invariantZipTuple3[A, B, C, Z]: InvariantZip.WithOut[(A, B, C), Z, (A, B, C, Z)] = ???

  implicit def invariantZipTuple4[A, B, C, D, Z]: InvariantZip.WithOut[(A, B, C, D), Z, (A, B, C, D, Z)] = ???
}

trait InvariantZipLowPriority1 {
  implicit def invariantZipAB[A, B]: InvariantZip.WithOut[A, B, (A, B)] = new InvariantZip[A, B] {
    type Out = (A, B)

    override def combine(a: A, b: B): (A, B) = ((a, b))

    override def projectLeft(o: Out): A = o._1

    override def projectRight(o: Out): B = o._2
  }
}
