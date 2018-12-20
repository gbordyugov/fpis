package fpis.chapter14

sealed trait ST[S,A] { self =>
  /*
   * note that the run() method is protected, contrarily to the normal
   * state monad
   */
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S,A](a: => A) = {
    lazy val memo: A = a
    new ST[S,A] {
      def run(s: S) = (memo, s)
    }
  }

  /*
   * for RunnableST[S,A] see below
   */
  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}

/*
 * observe that this trait is sealed and has no subclasses
 * it means that constructing instances of it is only possible by
 * calling the apply() method of the companion object
 */
sealed trait STRef[S,A] {
  protected var cell: A

  /*
   * methods read() and write() are pure: they don't do anything
   * themselves but rather return ST actions
   */
  def read: ST[S,A] = ST(cell)

  def write(a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  /*
   * we don't actually create any instances of STRef in apply() here
   * looks a bit counterintuitive, nicht wahr?
   *
   * what we create here is a ST[S,_] - it has the same type S as
   * the corresponding STRef[S,_] type. So this S type is a kind of
   * token or a pass to control who can access the state.
   */
  def apply[S,A](a: A): ST[S, STRef[S,A]] = {
    val ref: STRef[S,A] = new STRef[S,A] {
      var cell = a
    }
    ST(ref)
  }
}

trait RunnableST[A] {
  /*
   * here is the trick: we cannot choose A = STRef[X,Int] for any X,
   * since it is the caller of apply[S] that chooses a concrete S
   */
  def apply[S]: ST[S,A]
}


/*
 * Scala requires an implicit Manifest cor constructing arrays
 */
sealed abstract class STArray[S,A](implicit m: Manifest[A]) {
  protected def value: Array[A]

  def size: ST[S,Int] = ST(value.size)

  def write(i: Int, a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s:S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S,A] = ST(value(i))

  def freeze: ST[S,List[A]] = ST(value.toList)
}

object STArra {
  def apply[S,A:Manifest](sz: Int, v: A): ST[S,STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = Array.fill(sz)(v)
    })
}
