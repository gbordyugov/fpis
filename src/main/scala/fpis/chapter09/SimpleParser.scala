package fpis.chapter09

import scala.util.matching.Regex

object SimpleParserTest {
  trait Result[+A]

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  type Parser[+A] = Location => Result[A]
}

import SimpleParserTest._

object MyParsers extends Parsers[Parser] {
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???
  def delay[A](p: => Parser[A]): Parser[A] = ???
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???
  def or[A](p: Parser[A], q: => Parser[A]): Parser[A] = ???
  def regex(r: Regex): Parser[String] = ???
  def slice[A](p: Parser[A]): Parser[String] = ???
  implicit def string(s: String): Parser[String] = ???
}
