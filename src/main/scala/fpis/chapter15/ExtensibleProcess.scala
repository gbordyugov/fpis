package fpis.chapter15.ExtensibleProcess

import scala.language.higherKinds

trait Process[F[_],O] {
  import Process._

  def onHalt(f: Throwable => Process[F,O]): Process[F,O] = this match {
    case Halt(e) => Try(f(e))
    case Emit(h, t) => Emit(h, t.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
  }

  def ++(p: => Process[F,O]): Process[F,O] =
    this.onHalt{
      case End => p
      case err => Halt(err)
    }

  def flatMap[O2](f: O => Process[F,O2]): Process[F,O2] = this match {
    case Halt(err) => Halt(err)
    case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
    case Await(req,recv) => Await(req, recv andThen (_ flatMap f))
  }
}

object Process {
  case class Await[F[_],A,O](req: F[A],
    recv: Either[Throwable,A] => Process[F,O]) extends Process[F,O]

  case class Emit[F[_],O](head: O, tail: Process[F,O]) extends Process[F,O]

  case class Halt[F[_],O](err: Throwable) extends Process[F,O]

  /*
   * Normal termination
   */
  case object End extends Exception

  /*
   * Foreceful termination
   */
  case object Kill extends Exception

  def Try[F[_],O](p: Process[F,O]): Process[F,O] =
    try p
    catch {
      case e: Throwable => Halt(e)
    }

  /*
   * just a shortcut with curried arguments to help type inference
   */
  def await[F[_],A,O](req: F[A])
    (recv: Either[Throwable,A] => Process[F,O]): Process[F,O] =
    Await(req, recv)
}
