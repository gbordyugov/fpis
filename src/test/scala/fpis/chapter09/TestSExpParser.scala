package fpis.chapter09.SExpParser

import fpis.chapter09.SimpleParser.SimpleParsers


object AtomTest {
  val i = AtomInt(3)
  val s = AtomString("string")
}

object SExpParserTest {
  import SExpParser._
  val parser = sExpParser(SimpleParsers)
}

