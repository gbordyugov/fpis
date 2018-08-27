package fpis.chapter09.SimpleParser

import scala.util.matching.Regex

import fpis.chapter09.Parsers

class ParseError

case class SimpleParser[+A](p: String => ParseError) {
}

object TestingSimpleParser {
  val parsers: Parsers[ParseError, SimpleParser] = new Parsers[ParseError, SimpleParser] {
    def run[A](p: SimpleParser[A])(input: String): Either[ParseError, A] = ???
    def delay[A](p: => SimpleParser[A]): SimpleParser[A] = ???
    def flatMap[A, B](p: SimpleParser[A])(f: A => SimpleParser[B]): SimpleParser[B] = ???
    def or[A](p: SimpleParser[A], q: => SimpleParser[A]): SimpleParser[A] = ???
    def regex(r: Regex): SimpleParser[String] = ???
    def slice[A](p: SimpleParser[A]): SimpleParser[String] = ???
    implicit def string(s: String): SimpleParser[String] = ???
  }
}
