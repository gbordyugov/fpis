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
  def slice[A](p: Parser[A]): Parser[String] = ???

  implicit def string(s: String): Parser[String] = {
    val l = s.length
    loc => loc match {
      case Location(input, offset)
          if s == input.slice(offset, offset+l) => Success(s, l)
      case _ => Failure(ParseError(List((loc, "cannot parse string"))))
    }
  }
}
