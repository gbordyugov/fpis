package fpis.chapter15

import scala.collection.immutable.Stream

/*
 * Transforms a stream containing I values into a stream containing O values
 *
 * This is a state machine that tells a further component (the driver) what
 * to do next.
 *
 * So the driver consumes an instance of Process[I,O] *and* the input stream
 * of values I (and probably generates an output stream of O).
 */
sealed trait Process[I,O] {
  /*
   * Given p: Process[I,O] and in: Stream[I], the expression p(in)
   * produces a Stream[O]
   */
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt()      => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs      => recv(None)(xs)
    }
    case Emit(h, t)  => h #:: t(s)
  }

  def repeat: Process[I,O] = {
    def go(p: Process[I,O]): Process[I,O] = p match {
      case Halt()      => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i    => go(recv(i))
      }
      case Emit(h, t)  => Emit(h, go(t))
    }
    go(this)
  }
}

/*
 * Emit(head, tail) indicates to the driver that the `head` value
 * should be emitted to the output stream, and the machine should then
 * be transitioned to the `tail` state.
 */
case class Emit[I,O](head: O, tail: Process[I,O] = Halt[I,O])
    extends Process[I,O]

/*
 * Await(recv) requests a value from the input stream. The driver
 * should pass the next available value to the `recv` function, or
 * None if the input has no more elements.
 */
case class Await[I,O](recv: Option[I] => Process[I,O])
    extends Process[I,O]

/*
 * Halt indicates to the driver that no more elements should be read
 * from the input or emitted to the output.
 */
case class Halt[I,O]() extends Process[I,O]

/*
 * Companion object to Process trait
 *
 * contains useful utilities: smart constructors and combinators
 */
object Process {
  def liftOne[I,O](f: I => O): Process[I,O] =
    Await {
      case Some(i) => Emit(f(i))
      case None    => Halt()
    }

  def lift[I,O](f: I=>O): Process[I,O] = liftOne(f).repeat

  def filter[I](p: I=>Boolean): Process[I,I] =
    Await[I,I] {
      case Some(i) if p(i) => Emit(i)
      case _               => Halt()
    }.repeat

  def sum: Process[Double,Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit(d+acc, go(d+acc))
        case None    => Halt()
      }
    go(0.0)
  }
}
