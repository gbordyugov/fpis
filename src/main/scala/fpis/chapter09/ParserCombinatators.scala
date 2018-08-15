package fpis.chapter09

import fpis.chapter08._

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  /*
   * parses A and returns the digested portion of the string
   */
  def slice[A](p: Parser[A]): Parser[String]

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
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many(): Parser[List[A]] = self.many(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}

object ParserCombinators {
}
