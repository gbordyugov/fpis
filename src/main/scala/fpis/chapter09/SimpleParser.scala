package fpis.chapter09.SimpleParsers

import fpis.chapter09.{ParseError, Location, Parsers}

import scala.util.matching.Regex
import scala.language.higherKinds
import scala.language.implicitConversions


object SimpleParser {
  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] =
      this match {
        case Failure(e, ic) => Failure(f(e), ic)
        case _              => this
      }

    def attempt[A](p: Parser[A]): Parser[A] =
      l => p(l).uncommit

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _                => this
    }

    def addCommit(isCommited: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommited)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n+m)
      case _             => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  type Parser[+A] = Location => Result[A]
}


// case class Location(input: String, offset: Int = 0)
// case class ParseError(stack: List[(Location, String)])


/*
 * This can count as Exercise 9.13
 */
object SimpleParsers extends Parsers[SimpleParser.Parser] {
  import SimpleParser._

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input)) match {
      case Success(a, n)  => Right(a)
      case Failure(error, _) => Left(error)
    }

  def delay[A](p: => Parser[A]): Parser[A] = ???

  def succeed[A](a: A): Parser[A] =
    l => Success(a, 0)

  def fail: Parser[Any] =
    l => Failure(ParseError(List((l, "parser fail()"))), true)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    l => p(l) match {
      case Success(a, n)   => f(a)(l.advanceBy(n))
                                 .addCommit(n != 0)
                                 .advanceSuccess(n)
      case e@Failure(_, _) => e
    }

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    l => p(l).mapError(_.label(msg))

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    l => p(l).mapError(_.push(l, msg))

  def or[A](p: Parser[A], q: => Parser[A]): Parser[A] =
        l => p(l) match {
      case Failure(error, false) => q(l)
      case a                     => a
    }

  def regex(r: Regex): Parser[String] = {
    case l@Location(input, offset) =>
      r.findPrefixOf(input.drop(offset)) match {
        case Some(res) => Success(res, res.length)
        case None      =>
          Failure(ParseError(List((l, s"cannot parse regex $r"))), false)
      }
  }

  def slice[A](p: Parser[A]): Parser[String] = {
    case l@Location(input, offset) => p(l) match {
      case Success(_, n)   => Success(input.slice(offset, offset+n), n)
      case f@Failure(_, _) => f
    }
  }

  /*
   * Exercise 9.14 considered as done
   */
  implicit def string(s: String): Parser[String] = {
    case l@Location(input, offset) =>
      if (s == input.slice(offset, offset+s.length))
        Success(s, s.length)
      else
        Failure(l.toError(s"cannot parse string '$s'"), false)
  }
}
