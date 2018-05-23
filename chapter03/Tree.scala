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
}
