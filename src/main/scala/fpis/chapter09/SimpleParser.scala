package fpis.chapter09.SimpleParser

import scala.util.matching.Regex

import fpis.chapter09.Parsers

class SimpleParseError

case class SimpleParser[+A](run: String => Either[SimpleParseError, A]) {
}

object TestingSimpleParser {
  val parsers: Parsers[SimpleParseError, SimpleParser] = new Parsers[SimpleParseError, SimpleParser] {
    def run[A](p: SimpleParser[A])(input: String): Either[SimpleParseError, A] = p.run(input)
    def delay[A](p: => SimpleParser[A]): SimpleParser[A] = ???
    def flatMap[A, B](p: SimpleParser[A])(f: A => SimpleParser[B]): SimpleParser[B] = ???
    def or[A](p: SimpleParser[A], q: => SimpleParser[A]): SimpleParser[A] = ???
    def regex(r: Regex): SimpleParser[String] = ???
    def slice[A](p: SimpleParser[A]): SimpleParser[String] = ???
    implicit def string(s: String): SimpleParser[String] = ???
  }
}
