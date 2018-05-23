object TreeChapter{
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


  /*
   * Exercise 3.25
   */

  def size[A](t: Tree[A]): Long = t match {
    case Leaf(v) => 1
    case Branch(l, r) => size(l) + size(r)
  }


  /*
   * Exercise 3.26
   */

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x)      => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }


  /*
   * Exercise 3.27
   */

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }
}
