package fpis.chapter09.SimpleParsers

import org.scalatest._

class SimpleParsersTest extends FlatSpec {
  import SimpleParsers.{run => runParser, _}
  import SimpleParser.Parser

  "char parser" should "be able to parse a single char" in {
    assert(runParser(char('a'))("abc") == Right('a'))
  }

  "string parser" should "be able to parse simple strings" in {
    def abra: Parser[String] = "abra"
    assert (runParser(abra)("abra") === Right("abra"))
  }

  "many combinator" should "be able to parse zero occurences" in {
    def abra: Parser[List[String]] = "abra".many
    assert(runParser(abra)("") === Right(List(): List[String]))
  }

  it should "be able to parse one occurence" in {
    def abra: Parser[List[String]] = "abra".many
    assert(runParser(abra)("abra") === Right(List("abra")))
  }

  it should "be able to parse 100 occurence" in {
    def abra: Parser[List[String]] = "abra".many
    val hundredAbras = List.fill(100)("abra")
    assert(runParser(abra)(hundredAbras.mkString) === Right(hundredAbras))
  }

  "bla" should "blo" in {
    def abra: Parser[String] = "abra"

    val succ = runParser(abra)("abra")
    val fail = runParser(abra)("abr")

    def re: Parser[String] = regex("a+".r)

    val succ_re = runParser(re)("abra")
    val fail_re = runParser(re)("nopes")

    def number: Parser[String] = regex("[0-9]+".r)
    def a: Parser[String] = "a"
    def nAs: Parser[Int] = number.map(_.toInt)

    val flatMapSuccTest = runParser(number.map(_.toInt).flatMap(listOfN(_, a)))("4aaaa")
    val flatMapFailTest = runParser(number.map(_.toInt).flatMap(listOfN(_, a)))("4aaa")

    def manyA = "a".many
    val parseManyA  = runParser(manyA)("")
    // val parseMany1A = runParser("a".many1)("aaaaa")
  }
}
