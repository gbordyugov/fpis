package fpis.chapter15.ExtensibleProcess

import scala.language.higherKinds

import fpis.chapter13._

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

  def onComplete(p: => Process[F,O]): Process[F,O] = this.onHalt {
    case End => p.asFinalizer
    case err => p.asFinalizer ++ Halt(err)
  }

  def asFinalizer: Process[F,O] = this match {
    case Emit(h, t)       => Emit(h, t.asFinalizer)
    case Halt(e)          => Halt(e)
    case Await(req, recv) => await(req) {
      case Left(Kill) => this.asFinalizer
      case x          => recv(x)
    }
  }

  /*
   * Exercise 15.10
   */
  def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
    def go(cur: Process[F,O], acc: F[IndexedSeq[O]]): F[IndexedSeq[O]] =
      cur match {
        case Emit(h, t) => go(t, F.map(acc)(_ :+ h))
        case Halt(End) => acc
        case Halt(err) => F.fail(err)
        case Await(req, recv) =>
          val next = F.map(F.attempt(req))(recv)
          F.flatMap(next){n => go(n, acc)}
      }
    go(this, F.unit(IndexedSeq()))
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


  def runLog[O](src: Process[IO,O]): IO[IndexedSeq[O]] = IO {
    val E = java.util.concurrent.Executors.newFixedThreadPool(4)
    @annotation.tailrec
    def go(cur: Process[IO,O], acc: IndexedSeq[O]): IndexedSeq[O] =
      cur match {
        case Emit(h, t) => go(t, acc :+ h)
        case Halt(End) => acc
        case Halt(err) => throw err
        case Await(req, recv) =>
          val next =
            try recv(Right(unsafePerformIO(req)(E)))
            catch { case err: Throwable => recv(Left(err)) }
          go(next, acc)
      }
    try go(src, IndexedSeq())
    finally E.shutdown
  }

  def resource[R,O](acquire: IO[R])(use: R => Process[IO,O],
    release: R => Process[IO,O]): Process[IO,O] =
    await[IO,R,O](acquire)(r => use(r).onComplete(release(r)))
}
