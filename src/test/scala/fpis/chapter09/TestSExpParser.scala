package fpis.chapter09.SExpParser

import org.scalatest._

import fpis.chapter09.Location
import fpis.chapter09.SimpleParsers.SimpleParsers


object AtomTest {
  val i = AtomInt(3)
  val s = AtomString("string")
}

/*
class SExpParserTest extends FlatSpec {
  "bla" should "blo" in {
    import SExpParser._
    val parser = sExpParser(SimpleParsers)
    // println(parser(Location("symbol", 0)).toString())
  }
}
*/
