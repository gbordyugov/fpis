package fpis.chapter09.SExpParser

import scala.language.{higherKinds, implicitConversions}

import fpis.chapter09.{Parsers, ParseError, Location}
import fpis.chapter09.SimpleParser.SimpleParsers

object SExpParserTest {
  import SExpParser._
  val parser = sExpParser(SimpleParsers)
}

