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
}

sealed trait STRef[S,A] {
  protected var cell: A

  def read: ST[S,A] = ST(cell)

  def write(a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = {
    val ref: STRef[S,A] = new STRef[S,A] {
      var cell = a
    }
    ST(ref)
  }
}
