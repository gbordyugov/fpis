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

  def repeat: Process[F,O] = this ++ this.repeat

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

  import Process1.Process1

  def |>[O2](p2: Process1[O,O2]): Process[F,O2] = p2 match {
    case Halt(e) => this.kill onHalt { e2 => Halt(e) ++ Halt(e2) }
    case Emit(h, t) => Emit(h, this |> t)
    case Await(req, recv) => this match {
      case Halt(err) => Halt(err) |> recv(Left(err))
      case Emit(h, t) => t |> Try(recv(Right(h)))
      case Await(req0, recv0) => await(req0)(recv0 andThen (_ |> p2))
    }
  }

  //def pipe[O2](p2: Process[O,O2]): Process[F,O2] = this |> p2

  final def drain[O2]: Process[F,O2] = this match {
    case Halt(e) => Halt(e)
    case Emit(h, t) => t.drain
    case Await(req, recv) => Await(req, recv andThen(_.drain))
  }

  @annotation.tailrec
  final def kill[O2]: Process[F,O2] = this match {
    case Await(req, recv) => recv(Left(Kill)).drain.onHalt {
      case Kill => Halt(End)
      case e    => Halt(e)
    }
    case Halt(e) => Halt(e)
    case Emit(h, t) => t.kill
  }

  def filter(f: O=>Boolean): Process[F,O] =
    this |> Process1.filter(f)
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

  def emit[F[_],O](h: O, t: Process[F,O] = Halt[F,O](End)) = Emit(h, t)

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

  def resource[R,O](acquire: IO[R])(use: R => Process[IO,O])
    (release: R => Process[IO,O]): Process[IO,O] =
    await[IO,R,O](acquire) {
      case Left(e)  => Halt(e)
      case Right(r) => use(r).onComplete(release(r))
    }

  /*
   * Exercise 15.11
   */
  def eval[F[_],A](a: F[A]): Process[F,A] =
    await(a) {
      case Left(e) => Halt(e)
      case Right(a) => Emit(a, Halt(End))
    }

  def eval_[F[_],A,B](a: F[A]): Process[F,B] =
    await(a) {
      case Left(e) => Halt(e)
      case Right(a) => Halt(End)
    }

  def lines(filename: String): Process[IO,String] =
    resource { IO(io.Source.fromFile(filename)) } { src =>
      lazy val iter = src.getLines
      def step = if (iter.hasNext)
        Some(iter.next)
      else
        None
      lazy val lines: Process[IO,String] = eval(IO(step)) flatMap {
        case None       => Halt(End)
        case Some(line) => Emit(line, lines)
      }
      lines
    } { src =>
      eval_ { IO(src.close) }
    }
}

/*
 * the inner trait f fixes X to be I
 * Scala will complain on `is[I]#f[A]` unless A = I
 */
case class Is[I]() {
  sealed trait f[X]
  val Get = new f[I] {}
}

object Process1 {
  import Process._

  type Process1[I,O] = Process[Is[I]#f,O]

  def Get[I] = Is[I]().Get
  /*
   * substitution in the definition of Await results in
   * case class Await[A,O] (req: Is[I]#f[A],
                            recv: Either[Throwable,A] => Process[Is[I]#f,O])
       extends Process[Is[I]#f,O]
   */

  def await1[I,O](recv: I => Process1[I,O],
    fallback: Process1[I,O] = halt1[I,O]): Process1[I,O] =
    Await(Get[I], (e: Either[Throwable,I]) => e match {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(i)  => Try(recv(i))
    })

  def emit1[I,O](h: O, t1: Process1[I,O] = halt1[I,O]): Process1[I,O] =
    emit(h, t1)

  def halt1[I,O]: Process1[I,O] = Halt[Is[I]#f,O](End)

  def lift[I,O](f: I=>O): Process1[I,O] =
    await1[I,O](i => emit(f(i))).repeat

  def filter[I](f: I=>Boolean): Process1[I,I] =
    await1[I,I](i => if (f(i)) emit(i) else halt1).repeat

  /*
   * Exercise 15.12
   */
  def join[F[_],O](p: Process[F, Process[F,O]]): Process[F,O] =
    p.flatMap(x => x)
}

object T {
  import Process._

  def identity[A](a: A): A = a

  case class T[I1,I2]() {
    sealed trait f[X] { def get: Either[I1=>X,I2=>X] }
    val L = new f[I1] { def get =  Left(identity _) }
    val R = new f[I2] { def get = Right(identity _) }
  }

  def L[I1,I2] = T[I1,I2]().L
  def R[I1,I2] = T[I1,I2]().R

  type Tee[I1,I2,O] = Process[T[I1,I2]#f,O]

  def haltT[I1,I2,O]: Tee[I1,I2,O] = Halt[T[I1,I2]#f,O](End)

  def awaitL[I1,I2,O](recv: I1 => Tee[I1,I2,O],
    fallback: => Tee[I1,I2,O] = haltT[I1,I2,O]): Tee[I1,I2,O] =
    await[T[I1,I2]#f,I1,O](L) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a)  => Try(recv(a))
    }

  def awaitR[I1,I2,O](recv: I2 => Tee[I1,I2,O],
    fallback: => Tee[I1,I2,O] = haltT[I1,I2,O]): Tee[I1,I2,O] =
    await[T[I1,I2]#f,I2,O](R) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a)  => Try(recv(a))
    }

  def emitT[I1,I2,O](h: O, t1: Tee[I1,I2,O]=haltT[I1,I2,O]): Tee[I1,I2,O] =
    emit(h, t1)

  def zipWith[I1,I2,O](f: (I1,I2) => O): Tee[I1,I2,O] =
    awaitL[I1,I2,O](i1 =>
    awaitR[I1,I2,O](i2 => emitT(f(i1,i2)))).repeat
}
