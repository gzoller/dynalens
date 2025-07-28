package co.blocke.dynalens

import fastparse._
import fastparse.NoWhitespace._

object PathParserTest:

//  def path[$: P]: P[String] =
//    P(
//      CharsWhileIn("a-zA-Z0-9_.[]()").rep(1).!.map { full =>
//        val parenIdx = full.indexOf('(')
//        if parenIdx == -1 then
//          full
//        else {
//          val methodDotIdx = full.lastIndexOf('.', parenIdx)
//          if methodDotIdx == -1 then
//            full
//          else {
//            val chop = full.take(methodDotIdx)
//
//            // Now preserve [] if it's part of the last valid identifier
//            val remaining = full.drop(methodDotIdx)
//            val arraySuffix = if chop.endsWith("[]") || remaining.startsWith("[]") then "[]" else ""
//
//            chop + arraySuffix
//          }
//        }
//      }
//    )

  def path[$: P]: P[(String, Option[String])] = {

    def fullPath[$: P]: P[String] =
      P(CharsWhileIn("a-zA-Z0-9_.[]").rep(1).!)//.map{h=>println("S: "+h);h}

    // Non-consuming lookahead: detects if next char is '('
    def isFunc[$: P]: P[Boolean] =
      P(
        &("(").map(_ => true)
          | Pass.map(_ => false)
      )

    // Combine an
    // //.map{x => println("S2: "+x);x}d post-process
    (fullPath ~ isFunc ).map {
      case (raw, hasFunc) =>
        println(s"Raw: $raw  HasFunc: $hasFunc")
        if hasFunc then
          val lastDot = raw.lastIndexOf('.')
          if lastDot == -1 then
            (raw, None)
          else
            val base = raw.take(lastDot)
            val fn = raw.drop(lastDot + 1)
            (base, Some(fn))
        else
          (raw, None)
    }
  }

  def captureRemaining[$: P]: P[String] =
    P(AnyChar.rep.!)

  def pathWithRemainder[$: P]: P[((String,Option[String]), String)] =
    P(for {
      p <- path
      r <- captureRemaining
    } yield (p, r))


  def main(args: Array[String]): Unit =

    val tests = List(
      "foo",
      "foo.bar",
      "foo.bar.baz",
      "foo.bar[]",
      "foo.bar[].baz",
      "foo.bar[].baz.trim()",
      "foo.bar[].baz.trim(this.qty < 4).blah()",
      "foo1._baz3[].quux",
      "a.b[].c.d[].e"
    )

    tests.foreach { test =>
      println(s"=== Testing: $test ===")
      val result = parse(test, test => pathWithRemainder(using test))
      result match
        case Parsed.Success((pathValue, remaining), _) =>
          println(s"✅ Path: '$pathValue'")
          println(s"📌 Remaining: '$remaining'")
        case f: Parsed.Failure =>
          println(s"❌ Failed: ${f.msg}")
    }
