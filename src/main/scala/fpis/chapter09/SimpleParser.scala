package fpis.chapter09

import scala.util.matching.Regex

object SimpleParserTest {
  trait Result[+A]

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  type Parser[+A] = Location => Result[A]
}

import SimpleParserTest._

// case class Location(input: String, offset: Int = 0)
// case class ParseError(stack: List[(Location, String)])

object MyParsers extends Parsers[Parser] {
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input)) match {
      case Success(a, n)  => Right(a)
      case Failure(error) => Left(error)
    }

  def delay[A](p: => Parser[A]): Parser[A] = ???
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???
  def or[A](p: Parser[A], q: => Parser[A]): Parser[A] = ???
  def regex(r: Regex): Parser[String] = ???

  def slice[A](p: Parser[A]): Parser[String] = loc => p(loc) match {
    case Success(_, n) => Success(loc.input.slice(loc.offset, loc.offset+n), n)
    case Failure(error) => Failure(error)
  }

  implicit def string(s: String): Parser[String] =
    loc =>
  if (s == loc.input.slice(loc.offset, loc.offset+s.length))
    Success(s, s.length)
  else
    Failure(ParseError(List((loc, "cannot parse string"))))
}
