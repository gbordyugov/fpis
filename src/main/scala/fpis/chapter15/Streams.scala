package fpis.chapter15

import scala.collection.immutable.Stream

/*
 * Transforms a stream containing I values into a stream containing O values
 *
 * This is a state machine that tells a further component (the driver) what
 * to do next.
 *
 * So the driver (the apply() method below) consumes an instance of
 * Process[I,O] *and* the input stream of values I (and probably
 * generates an output stream of O).
 */
sealed trait Process[I,O] {
  /*
   * Given p: Process[I,O] and in: Stream[I], the expression p(in)
   * produces a Stream[O]. This is the driver mentioned above.
   */
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt()      => Stream()
    case Await(recv) => s match {
      /*
       * If there is at least one element in the stream, consume it by
       * calling recv() on it. The result of this consumption will be
       * a new Process, which is then applied to the tail of the
       * stream.
       */
      case h #:: t => recv(Some(h))(t)
      /*
       * if the stream is empty, call recv() with None, it will
       * return a Process, apply it to the (empty) xs again
       */
      case xs      => recv(None)(xs)
    }
    /*
     * Emit doesn't consume elements of the stream s, it just emitcs
     * a new value.
     *
     * Looks like a return/unit to me!
     */
    case Emit(h, t)  => h #:: t(s)
  }

  def repeat: Process[I,O] = {
    /*
     * I don't quite understand what this function does exactly, but
     * here are some observations:
     *  - it replaces every appearance of p: Process[I,O] by go(p)
     *  - except if the source stream has been exhausted (the Await
     * case)
     *  - when process comes to a Halt, it restarts it by calling
     * go(this)
     */
    def go(p: Process[I,O]): Process[I,O] = p match {
      /*
       * if we wound up in a Halt() state, restart from `this`
       */
      case Halt()      => go(this)
      case Await(recv) => Await {
        /*
         * pass None to the old handler `recv`, do not restart if the
         * source stream is exhausted
         */
        case None => recv(None)
        /*
         * I don't understand this recursion, why not simply recv(i)?
         */
        case i    => go(recv(i))
      }
      /*
       * keep head, but recurse on tail thus making tail restartable
       */
      case Emit(h, t)  => Emit(h, go(t))
    }
    go(this)
  }

  /*
   * Exercise 15.5
   */
  def |>[O2](p2: Process[O,O2]): Process[I,O2] = this match {
    case Halt()      => Halt()
    case Emit(h,t)   => ???
    case Await(recv) => ???
  }
}

/*
 * Halt indicates to the driver that no more elements should be read
 * from the input or emitted to the output.
 */
case class Halt[I,O]() extends Process[I,O]

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

  def emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]()) =
    Emit(head, tail)

  /*
   * Exercise 15.0
   */
  def echo[I]: Process[I,I] = lift[I,I](a => a)

  /*
   * Exercise 15.1
   */
  def take[I](n: Int): Process[I,I] =
    if (n == 0)
      Halt()
    else
      await(i => emit(i, take(n-1)))

  def takeOld[I](n: Int): Process[I,I] = {
    def go(k: Int): Process[I,I] = Await {
      case Some(i) if (k > 0) => Emit(i, go(k-1))
      case _                  => Halt()
    }
    go(n)
  }

  def drop[I](n: Int): Process[I,I] =
    if (n > 0)
      await(i => drop(n-1))
    else
      echo[I]

  def dropOld[I](n: Int): Process[I,I] = {
    def go(k: Int): Process[I,I] = {
      def p: Process[I,I] = Await {
        case Some(i) if (k == 0) => Emit(i)
        case Some(i)             => go(k-1)
        case None                => Halt()
      }
      p.repeat
    }
    go(n)
  }

  def takeWhile[I](f: I=>Boolean): Process[I,I] =
    await(i => if (f(i)) emit(i, takeWhile(f)) else Halt())

  def takeWhileOld[I](f: I=>Boolean): Process[I,I] = {
    def go: Process[I,I] = Await {
      case Some(i) if (f(i)) => Emit(i, go)
      case _                 => Halt()
    }
    go
  }

  def dropWhile[I](f: I=>Boolean): Process[I,I] =
    await(i => if (f(i)) dropWhile(f) else emit(i, echo))

  def dropWhileOld[I](f: I=>Boolean): Process[I,I] = {
    def go: Process[I,I] = {
      def p: Process[I,I] = Await {
        case Some(i) if (!f(i)) => Emit(i, go)
        case _                  => Halt()
      }
      p.repeat
    }
    go
  }

  /*
   * Exercise 15.2
   */
  def count: Process[Double,Int] = {
    def go(n: Int): Process[Double, Int] =
      Await {
        case Some(d) => Emit(n+1, go(n+1))
        case None    => Halt()
      }
    go(0)
  }

  /*
   * Exercise 15.3
   */
  def mean: Process[Double,Double] = {
    def go(n: Int, sum: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit((d+sum)/(n+1), go(n+1, sum+d))
        case None    => Halt()
      }
    go(0, 0.0)
  }

  def await[I,O](f: I => Process[I,O],
    fallback: Process[I,O] = Halt[I,O]()): Process[I,O] =
    Await[I,O] {
      case Some(i) => f(i)
      case None    => fallback
    }

  def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] =
    await((i: I) => f(i,z) match {
      case (o, s2) => Emit(o, loop(s2)(f))
    })

  /*
   * Exerise 15.4
   */
  def sumViaLoop: Process[Double,Double] =
    loop[Double,Double,Double](0.0)((i, s) => (i+s, i+s))

  def meanViaLoop: Process[Double,Double] =
    loop[(Int,Double),Double,Double]((0, 0.0)) {
      case (i, (n, s)) => ((i+s)/(n+1), (n+1, s+i))
    }
}
