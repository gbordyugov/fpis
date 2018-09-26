package fpis.chapter09

import scala.util.matching.Regex
import scala.language.higherKinds
import scala.language.implicitConversions


object SimpleParserTest {
  trait Result[+A]

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  type Parser[+A] = Location => Result[A]
}


import SimpleParserTest._

// case class Location(input: String, offset: Int = 0)
// case class ParseError(stack: List[(Location, String)])


/*
 * This can count as Exercise 9.13
 */
object MyParsers extends Parsers[Parser] {
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input)) match {
      case Success(a, n)  => Right(a)
      case Failure(error) => Left(error)
    }

  def delay[A](p: => Parser[A]): Parser[A] = ???

  def succeed[A](a: A): Parser[A] = l => Success(a, 0)

  def fail: Parser[Any] = l => Failure(ParseError(List((l, "parser fail()"))))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???
  /*
    l => p(l) match {
      case Success(a, n) => f(a)(Location(l.input, l.offset+n))
      case Failure(get)  => Failure(get)
    }
   */

  def label[A](msg: String)(p: Parser[A]): Parser[A] = l => p(l) match {
    case Failure(ParseError(error)) =>
      Failure(ParseError((l, msg)::error.tail)) // replace head
    case s@Success(_, _) => s
  }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] = l => p(l) match {
    case Failure(ParseError(error)) =>
      Failure(ParseError((l, msg)::error)) // just extend
    case s@Success(_, _) => s
  }

  def or[A](p: Parser[A], q: => Parser[A]): Parser[A] = l => p(l) match {
    case Failure(error) => q(l)
    case a              => a
  }

  def regex(r: Regex): Parser[String] = loc =>
  r.findPrefixOf(loc.input.drop(loc.offset)) match {
    case Some(res) => Success(res, res.length)
    case None      => Failure(ParseError(List((loc, s"cannot parse regex $r"))))
  }

  def slice[A](p: Parser[A]): Parser[String] = l => p(l) match {
    case Success(_, n) => Success(l.input.slice(l.offset, l.offset+n), n)
    case Failure(error) => Failure(error)
  }

  implicit def string(s: String): Parser[String] =
    loc =>
  if (s == loc.input.slice(loc.offset, loc.offset+s.length))
    Success(s, s.length)
  else
    Failure(loc.toError(s"cannot parse string $s"))
}


object TestMyParsers {
  import MyParsers._

  def abra: Parser[String] = "abra"

  val succ = run(abra)("abra")
  val fail = run(abra)("abr")

  def re: Parser[String] = regex("a+".r)

  val succ_re = run(re)("abra")
  val fail_re = run(re)("nopes")

  def number: Parser[String] = regex("[0-9]+".r)
  def a: Parser[String] = "a"
  def nAs: Parser[Int] = number.map(_.toInt)

  val flatMapSuccTest = run(number.map(_.toInt).flatMap(listOfN(_, a)))("4aaaa")
  val flatMapFailTest = run(number.map(_.toInt).flatMap(listOfN(_, a)))("4aaa")
}
