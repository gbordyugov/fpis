package fpis.chapter09.SimpleParser
import fpis.chapter09.Parsers

class ParseError

case class SimpleParser[+A](p: String => ParseError) {
}

object TestingSimpleParser {
  val parsers: Parsers[ParseError, SimpleParser] = ???
}
