package fpis.chapter09

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  /*
   * OK, here's something going on: this one promotes a string to a
   * parser...
   */
  implicit def string(s: String): Parser[String]

  /*
   * ... and we would like to promote a parser to ParserOps,
   * but it wouldn't go, since scala doesn't chain implicit
   * convertions...
   */
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  /*
   * ... except if the convertion function itself has an implicit
   * parameter. We use it here to make ParserOps[String] out of String
   *
   * see https://stackoverflow.com/questions/5332801/how-can-i-chain-implicits-in-scala/5332804
   */
  implicit def asStringParser[A](a: A)
    (implicit f: A => Parser[String]): ParserOps[String] =
      ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  case class ParserOps[A](p: Parser[A]) {
    def  |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }
}

object ParserCombinators {
}
