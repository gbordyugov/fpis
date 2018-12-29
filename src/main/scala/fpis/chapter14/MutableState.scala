package fpis.chapter14

object Quicksort {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray

    def swap(i: Int, j: Int): Unit = {
      val tmp = arr(i)
      arr(i) = arr(j)
      arr(j) = tmp
    }

    /*
     * (n, r) are the inclusive index boundaries
     */
    def partition(n: Int, r: Int, pivot: Int): Int = {
      /*
       * put pivot to the right
       */
      val pivotVal = arr(pivot)
      swap(pivot, r)

      /*
       * index of the first array element which is >= pivot
       */
      var j = n

      /*
       * up to, but not including, r
       */
      for (i <- n until r)
        if (arr(i) < pivotVal) {
          swap(i, j)
          j += 1
        }

      /*
       * put pivot back in place
       */
      swap(j, r)

      j
    }

    def choosePivot(n: Int, r: Int): Int =
      (n + r) / 2

    def qs(n: Int, r: Int): Unit =
      if (n < r) {
        val pivot = choosePivot(n, r)
        val pi = partition(n, r, pivot)
        qs(n, pi - 1)
        qs(pi + 1, r)
      }

    qs(0, arr.length - 1)
    arr.toList
  }
}

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


  /*
   * Exercise 14.1
   */
  def fill(xs: Map[Int,A]): ST[S,Unit] =
    xs.toList.foldLeft(ST[S,Unit](())) {
      case (st, (ix, value)) => st.flatMap(_ => write(ix, value))
    }

  def swap(i: Int, j: Int): ST[S,Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {
  def apply[S,A:Manifest](sz: Int, v: A): ST[S,STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = Array.fill(sz)(v)
    })

  def fromList[S,A:Manifest](xs: Seq[A]): ST[S,STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = xs.toArray
    })
}

/*
 * Exercise 14.2
 */
object Exercise1402 {
  def partition[S](arr: STArray[S,Int], n: Int, r: Int,
    pivot: Int): ST[S,Int] = {

    var noop = ST[S,Unit](())

    for {
      p <- arr.read(pivot)
      _ <- arr.swap(pivot, r)
      j <- STRef(n)
      _ <- (n until r).foldLeft(noop) { (acc, i) =>
        for {
          _ <- acc // this bloody line seems bogus, but IS important
          a <- arr.read(i)
          _ <- if (a < p) for {
            k <- j.read
            _ <- arr.swap(i, k)
            _ <- j.write(k + 1)
          } yield ()
          else noop
        } yield ()
      }
      l <- j.read
      _ <- arr.swap(l, r)
    } yield l
  }


  def choosePivot(n: Int, r: Int): Int =
    (n + r) / 2

  def qs[S](arr: STArray[S,Int], n: Int, r: Int): ST[S,Unit] =
    if (n < r) {
      val pivot = choosePivot(n, r)
      for {
        p <- partition(arr, n, r, pivot)
        _ <- qs(arr, n, p - 1)
        _ <- qs(arr, p + 1, r)
      } yield ()
    }
    else {
      ST(())
    }

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty)
      xs
    else
      ST.runST(new RunnableST[List[Int]] {
        def apply[S] = for {
          arr    <- STArray.fromList(xs)
          size   <- arr.size
          _      <- qs(arr, 0, size-1)
          sorted <- arr.freeze
        } yield sorted
      })
}

/*
 * Exercise 14.3
 */
sealed abstract class STMap[S,K,V] {
  import scala.collection.mutable.HashMap
  protected def hmap: HashMap[K,V]

  def contains[S](key: K): ST[S,Boolean] = ST(hmap.contains(key))

  def get[S](key: K): ST[S,Option[V]] = ST(hmap.get(key))

  def put[S](key: K, value: V): ST[S,Unit] =
    ST(hmap.put(key, value)).flatMap(_ => ST(()))
}

object STMap {
  import scala.collection.mutable.HashMap
  def apply[S,K,V](items: (K, V)*) = {
    val hm = new HashMap[K,V]
    items.foreach(x => hm.put(x._1, x._2))

    new STMap[S,K,V] {
      val hmap = hm
      }
    }
}
