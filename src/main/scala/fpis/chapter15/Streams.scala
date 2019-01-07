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
   * So, given p: Process[I,O] and in: Stream[I], the expression p(in)
   * *produces a Stream[O]
   */
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs      => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
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

