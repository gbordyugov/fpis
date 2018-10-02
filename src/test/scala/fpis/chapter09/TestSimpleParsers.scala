package fpis.chapter09.SimpleParsers

import org.scalatest._

class SimpleParsersTest extends FlatSpec {
  "bla" should "blo" in {
    import SimpleParsers.{run => parserRun, _}
    import SimpleParser.Parser

    def abra: Parser[String] = "abra"

    val succ = parserRun(abra)("abra")
    val fail = parserRun(abra)("abr")

    def re: Parser[String] = regex("a+".r)

    val succ_re = parserRun(re)("abra")
    val fail_re = parserRun(re)("nopes")

    def number: Parser[String] = regex("[0-9]+".r)
    def a: Parser[String] = "a"
    def nAs: Parser[Int] = number.map(_.toInt)

    val flatMapSuccTest = parserRun(number.map(_.toInt).flatMap(listOfN(_, a)))("4aaaa")
    val flatMapFailTest = parserRun(number.map(_.toInt).flatMap(listOfN(_, a)))("4aaa")

    def manyA = "a".many
    val parseManyA  = parserRun(manyA)("")
    // val parseMany1A = parserRun("a".many1)("aaaaa")
  }
}
