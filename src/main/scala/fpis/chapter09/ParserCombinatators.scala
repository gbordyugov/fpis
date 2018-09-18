package fpis.chapter09

import fpis.chapter08._

import scala.util.matching._

case class Location(input: String, offset: Int = 0)
case class ParseError(stack: List[(Location, String)])

trait Parsers[Parser[+_]] { self =>

  def errorLocation(e: ParseError): Location = ???
  def errorMessage(e: ParseError): String = ???

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  /*
   * primitive
   */
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  /*
   * parses A and returns the digested portion of the string
   * primitive
   */
  def slice[A](p: Parser[A]): Parser[String]


  /*
   * Exercise 9.1
   *
   * Re-defined in terms of flatMap() as part of Exercise 9.7
   */
  /*
   def map2[A,B,C](a: Parser[A], b: => Parser[B])
   (f: (A, B) => C): Parser[C] =
   map(product(a, b)){case (a, b) => f(a, b)}
   */

  /*
   * Exercise 9.3
   */
  def many[A](p: Parser[A]): Parser[List[A]] =
    or(map2(p, many(p))(_ :: _), succeed(List(): List[A]))

  /*
   * Exercise 9.4
   */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case 0 => succeed(List(): List[A])
    case m => map2(p, listOfN(n - 1, p))(_ :: _)
  }

  /*
   * Exercise 9.5
   */
  def delay[A](p: => Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  /*
   * Exercise 9.6
   * primitive
   */
  implicit def regex(r: Regex): Parser[String]

  def nAs: Parser[(Int, List[Char])] = {
    val regexParser = "[0-9]+".r
    val intParser = map(regexParser)(_.toInt)
    flatMap(intParser) { n =>
      map(listOfN(n, char('a'))) { chars => (n, chars) }
    }
  }

  /*
   * Exercise 9.7
   */
  def product[A, B](a: Parser[A], b: Parser[B]): Parser[(A, B)] =
    flatMap(a)(a => map(b)(b => (a, b)))

  def map2[A, B, C](a: Parser[A], b: Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(a)(a => map(b)(b => f(a, b)))

  /*
   * Exercise 9.8
   */
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(a => succeed(f(a)))

  /*
   * OK, here's something going on: this one promotes a string to a
   * parser...
   * primitive
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
   *
   * in the light of the method string() above, the type A will be
   * String most of the times
   */
  implicit def asStringParser[A](a: A)
    (implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  /*
   * Re-defined in Exercise 9.4
   */
  // def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  /*
   * convenience combinators
   */

  /*
   * Sequences two parsers, ignoring the result of the first or the
   * second. We wrap the ignored half in slice, since we don't care
   * about its result. It is lazy in the second argument since map2()
   * is lazy in its second arg too.
   */
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_,b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a,b) => a)

  /*
   * the purpose of this class is that Parser[A] can be automagically
   * promoted to a ParserOps[A], using the implicit function above
   */
  case class ParserOps[A](p: Parser[A]) {
    def  |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def product[B](p2: Parser[B]) = self.product(p, p2)
    def      **[B](p2: Parser[B]) = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def *>[B](q: => Parser[B]  ): Parser[B] = self.skipL(p, q)
    def <*   (q: => Parser[Any]): Parser[A] = self.skipR(p, q)
  }

  /*
   * this one is used for testing
   */
  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    /*
     * Exercise 9.2
     */
    def productLaw[A, B, C](a: Parser[A], b: Parser[B], c: Parser[C])
      (in: Gen[String]): Prop = {
      def flattenL[A, B, C](a: ((A, B), C)): (A, B, C) = a match {
        case ((a, b), c) => (a, b, c)
      }

      def flattenR[A, B, C](a: (A, (B, C))): (A, B, C) = a match {
        case (a, (b, c)) => (a, b, c)
      }
      val lft: Parser[(A, B, C)] = ((a ** b) ** c).map(flattenL)
      val rgt: Parser[(A, B, C)] = (a ** (b ** c)).map(flattenR)
      equal(lft, rgt)(in)
    }

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}
