package co.blocke.dynalens

import fastparse._
import fastparse.NoWhitespace._

object PathParser {
  def identS[$: P]: P[String] =
    P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_").rep).!

  def identU[$: P]: P[Unit] =
    P(CharIn("a-zA-Z_") ~ CharsWhileIn("a-zA-Z0-9_").rep)

  def indexPart[$: P]: P[String] =
    P("[" ~ CharsWhileIn("0-9").! ~ "]").map(i => s"[$i]")

  def wildcardIndex[$: P]: P[String] =
    P("[]").!

  def curlyBraces[$: P]: P[String] =
    P("{}").!

  def optSuffix[$: P]: P[String] =
    P("?".!.?).map(_.getOrElse(""))

  def segment[$: P]: P[String] =
    P(identS ~ (wildcardIndex | indexPart | curlyBraces).? ~ optSuffix).map {
      case (name, Some(suffixPart), suf) => s"$name$suffixPart$suf"
      case (name, None, suf)             => s"$name$suf"
    }

  def pathBase[$: P]: P[String] =
    P(segment ~ (!("." ~ identU ~ "(") ~ "." ~ segment).rep).map {
      case (head, tail) => (head +: tail.toList).mkString(".")
    }
}

import PathParser.*

object ParseDemo {
  def runOne(input: String): Unit = {
    val res = parse(input, (p: P[Any]) => pathBase(using p))
    res match {
      case f: Parsed.Failure =>
        println(s"❌ '$input'\n    ${f.trace().longMsg}\n")

      case Parsed.Success(base, index) =>
        val rest = input.substring(index)
        println(
          s"""✅ '$input'
             |    base:   $base
             |    next:   '$rest'
             |""".stripMargin
        )
    }
  }

  def main(args: Array[String]): Unit = {
    val samples = List(
      "foo",
      "foo.bar",
      "foo[]",
      "foo[3]",
      "foo{}.id",
      "foo[].bar",
      "foo[].bar.filter(x > 0)",
      "foo[].bar.filter(x > 0).sortAsc(num)",
      "orders[].items[].qty.sum()",
      "items[].qty?.isDefined()",
      "a.b[10].c{}.d.filter(this.n>0)",
      "items[].qty"
    )
    samples.foreach(runOne)
  }
}