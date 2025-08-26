/*
 * Copyright (c) 2025 Greg Zoller
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package co.blocke.dynalens

import zio.*
import zio.test.*

import DynaLens.*
import parser.Script

import CtxStrings.*

object Parsing extends ZIOSpecDefault:

  def spec = suite("Parsing Tests")(
    test("Simple val assignment script test") {
      val script =
        """
          |  val x = 42
          |  val y = x + 8
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,ConstantFn(42)), ValStmt(y,AddFn(GetFn(x),ConstantFn(8)))))"""
      val expectedResult = """top -> Item(abc,2,5)
                             |x -> 42
                             |y -> 50""".stripMargin + "\n"
      val inst = Item("abc", 2, 5)
      val a = dynalens[Item]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Path and operator precedence") {
      val script =
        """
          |  val x = 3 + items[1].num * 2
          |  val y = (3 + items[1].num) * 2
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,AddFn(ConstantFn(3),MultiplyFn(GetFn(items[1].num),ConstantFn(2)))), ValStmt(y,MultiplyFn(AddFn(ConstantFn(3),GetFn(items[1].num)),ConstantFn(2)))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,2,5), Item(xyz,1,7)),1)
                             |x -> 17
                             |y -> 20""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Map statement") {
      val script =
        """
          |  items[].num => 99/3
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(items[].num,DivideFn(ConstantFn(99),ConstantFn(3)))))"""
      val expectedResult = "top -> Shipment(aaa,List(Item(abc,2,33), Item(xyz,1,33)),1)\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa", List(Item("abc", 2, 33), Item("xyz", 1, 33)), 1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Update statement") {
      val script =
        """
          |  items[0].num = 1
          |  num = num * 5
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(UpdateStmt(items[0].num,ConstantFn(1)), UpdateStmt(num,MultiplyFn(GetFn(num),ConstantFn(5)))))"""
      val expectedResult = "top -> Shipment(aaa,List(Item(abc,2,1), Item(xyz,1,7)),5)\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa", List(Item("abc", 2, 1), Item("xyz", 1, 7)), 5) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Non-empty statement BlockFn") {
      val script =
        """
        |  items[0].num = {
        |    val z = "wow"
        |    val y = 1
        |    y
        |  }
        |""".stripMargin

      val expectedCompiled = """BlockStmt(List(UpdateStmt(items[0].num,BlockFn(List(ValStmt(z,ConstantFn(wow)), ValStmt(y,ConstantFn(1))),GetFn(y)))))"""
      val expectedResult = "top -> Shipment(aaa,List(Item(abc,2,1), Item(xyz,1,7)),1)\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa", List(Item("abc", 2, 1), Item("xyz", 1, 7)), 1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Empty statement BlockFn") {
      val script =
        """
          |  items[0].num = {
          |    1
          |  }
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(UpdateStmt(items[0].num,BlockFn(List(),ConstantFn(1)))))"""
      val expectedResult = "top -> Shipment(aaa,List(Item(abc,2,1), Item(xyz,1,7)),1)\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa", List(Item("abc", 2, 1), Item("xyz", 1, 7)), 1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("If statement-no else") {
      val script =
        """
          |  if items[0].num >= 0 then
          |    items[0].num = 12
          |  if items[1].num < 5 then
          |    items[1].num = 99
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(IfStmt(GreaterThanOrEqualFn(GetFn(items[0].num),ConstantFn(0)),UpdateStmt(items[0].num,ConstantFn(12)),None), IfStmt(LessThanFn(GetFn(items[1].num),ConstantFn(5)),UpdateStmt(items[1].num,ConstantFn(99)),None)))"""
      val expectedResult = "top -> Shipment(aaa,List(Item(abc,2,12), Item(xyz,1,7)),1)\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa", List(Item("abc", 2, 12), Item("xyz", 1, 7)), 1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("If statement-with else") {
      val script =
        """
          |  if items[0].num >= 0 then
          |    items[0].num = 12
          |  if items[1].num < 5 then
          |    items[1].num = 99
          |  else {
          |    val z = -1
          |    items[1].num = z
          |  }
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(IfStmt(GreaterThanOrEqualFn(GetFn(items[0].num),ConstantFn(0)),UpdateStmt(items[0].num,ConstantFn(12)),None), IfStmt(LessThanFn(GetFn(items[1].num),ConstantFn(5)),UpdateStmt(items[1].num,ConstantFn(99)),Some(BlockStmt(List(ValStmt(z,NegateFn(ConstantFn(1))), UpdateStmt(items[1].num,GetFn(z))))))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,2,12), Item(xyz,1,-1)),1)
                             |z -> -1""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa", List(Item("abc", 2, 12), Item("xyz", 1, -1)), 1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("If fn") {
      val script =
        """
          |  val z = if items[0].num >= 0 then 2 else 3
          |  items[].qty => this * z
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(z,IfFn(GreaterThanOrEqualFn(GetFn(items[0].num),ConstantFn(0)),ConstantFn(2),ConstantFn(3))), MapStmt(items[].qty,MultiplyFn(GetFn(this),GetFn(z)))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,4,5), Item(xyz,2,7)),1)
                             |z -> 2""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa", List(Item("abc", 4, 5), Item("xyz", 2, 7)), 1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("If fn with blockFn") {
      val script =
        """
          | # My little script comment
          |  items[].qty => if this < 2 then 0 else {
          |    val y = items.num  # A comment
          |    y * 2
          |  }
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(MapStmt(items[].qty,IfFn(LessThanFn(GetFn(this),ConstantFn(2)),ConstantFn(0),BlockFn(List(ValStmt(y,GetFn(items[].num))),MultiplyFn(GetFn(y),ConstantFn(2)))))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,10,5), Item(xyz,0,7)),1)""" + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa", List(Item("abc", 10, 5), Item("xyz", 0, 7)), 1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("And, or, and Not") {
      val script =
        """
          |  val x = if items[0].num >= 2 && !(items[0].num > 10) || items[1].num == 8 then 7 else 1
          |  val y = true
          |  val z = !y
          |  if z then
          |    num = 22
          |  else
          |    num = x
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,IfFn(OrFn(AndFn(GreaterThanOrEqualFn(GetFn(items[0].num),ConstantFn(2)),NotFn(GreaterThanFn(GetFn(items[0].num),ConstantFn(10)))),EqualFn(GetFn(items[1].num),ConstantFn(8))),ConstantFn(7),ConstantFn(1))), ValStmt(y,BooleanConstantFn(true)), ValStmt(z,NotFn(toBooleanFn(GetFn(y)))), IfStmt(toBooleanFn(GetFn(z)),UpdateStmt(num,ConstantFn(22)),Some(UpdateStmt(num,GetFn(x))))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,2,5), Item(xyz,1,7)),7)
                             |x -> 7
                             |y -> true
                             |z -> false""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 7) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("String functions work") {
      val script =
        """
          |  val x = "FooBar  "
          |  val y = x.trim().toLowerCase().contains("oob")
          |  val z = x.toUpperCase()
          |  val q = x.matchesRegex("F.*?B.*")
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,ConstantFn(FooBar  )), ValStmt(y,ContainsFn(ToLowerFn(TrimFn(GetFn(x))),ConstantFn(oob))), ValStmt(z,ToUpperFn(GetFn(x))), ValStmt(q,MatchesRegexFn(GetFn(x),ConstantFn(F.*?B.*)))))"""
      def normalize(str: String): Set[String] = str.trim.linesIterator.map(_.trim).filter(_.nonEmpty).toSet
      val expectedResult = """top -> Item(abc,2,5)
                             |x -> FooBar
                             |y -> true
                             |z -> FOOBAR
                             |q -> true
                             |""".stripMargin + "\n"
      val inst = Item("abc", 2, 5)
      val a = dynalens[Item]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && normalize(resultStr) == normalize(expectedResult) && compiledScript.toString == expectedCompiled)
    },
    test("More String functions work") {
      val script =
        """
          |  val x = "FooBar"
          |  val y = x.substr(3)
          |  val s = 1
          |  val e = 3
          |  val z = x.substr(s,e)
          |  val q = "YooHoo"
          |  val r = q.replace("oo","aa")
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,ConstantFn(FooBar)), ValStmt(y,SubstringFn(GetFn(x),ConstantFn(3),None)), ValStmt(s,ConstantFn(1)), ValStmt(e,ConstantFn(3)), ValStmt(z,SubstringFn(GetFn(x),GetFn(s),Some(GetFn(e)))), ValStmt(q,ConstantFn(YooHoo)), ValStmt(r,ReplaceFn(GetFn(q),ConstantFn(oo),ConstantFn(aa)))))"""
      def normalize(str: String): Set[String] = str.trim.linesIterator.map(_.trim).filter(_.nonEmpty).toSet
      val expectedResult = """top -> Item(abc,2,5)
                             |x -> FooBar
                             |y -> Bar
                             |s -> 1
                             |e -> 3
                             |z -> oo
                             |q -> YooHoo
                             |r -> YaaHaa
                             |""".stripMargin + "\n"
      val inst = Item("abc", 2, 5)
      val a = dynalens[Item]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && normalize(resultStr) == normalize(expectedResult) && compiledScript.toString == expectedCompiled)
    },
    test("Even more String functions work") {
      val script =
        """
          |  val x = "Mike"
          |  val y = 12.45
          |  val t = "Hello {x}, your balance is ${y}"
          |  val u = t.template()
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,ConstantFn(Mike)), ValStmt(y,ConstantFn(12.45)), ValStmt(t,ConstantFn(Hello {x}, your balance is ${y})), ValStmt(u,InterpolateFn(GetFn(t),Map()))))"""
      def normalize(str: String): Set[String] = str.trim.linesIterator.map(_.trim).filter(_.nonEmpty).toSet
      val expectedResult = """top -> Item(abc,2,5)
                             |x -> Mike
                             |y -> 12.45
                             |t -> Hello {x}, your balance is ${y}
                             |u -> Hello Mike, your balance is $12.45""".stripMargin + "\n"
      val inst = Item("abc", 2, 5)
      val a = dynalens[Item]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && normalize(resultStr) == normalize(expectedResult) && compiledScript.toString == expectedCompiled)
    },
    test("String literals work") {
      val script =
        """
          |  val x = "Mike"
          |  val y = 12.4
          |  val z = "Blip".contains("li")
          |  val u = "Hello {x}, your balance is {y%.2f}".template()
          |  val q = "wow " :: y*2 :: number
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,ConstantFn(Mike)), ValStmt(y,ConstantFn(12.4)), ValStmt(z,ContainsFn(ConstantFn(Blip),ConstantFn(li))), ValStmt(u,InterpolateFn(ConstantFn(Hello {x}, your balance is {y%.2f}),Map(x -> GetFn(x), y -> GetFn(y)))), ValStmt(q,ConcatFn(List(ConstantFn(wow ), MultiplyFn(GetFn(y),ConstantFn(2)), GetFn(number))))))"""
      def normalize(str: String): Set[String] = str.trim.linesIterator.map(_.trim).filter(_.nonEmpty).toSet
      val expectedResult = """top -> Item(abc,2,5)
                             |x -> Mike
                             |y -> 12.4
                             |z -> true
                             |q -> wow 24.8abc
                             |u -> Hello Mike, your balance is 12.40""".stripMargin + "\n"
      val inst = Item("abc", 2, 5)
      val a = dynalens[Item]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && normalize(resultStr) == normalize(expectedResult) && compiledScript.toString == expectedCompiled)
    },
    test("Path expressions in string interpolation works") {
      val script =
        """
          |  val x = "Thingy = {items[1].num%.3f}".template()
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,InterpolateFn(ConstantFn(Thingy = {items[1].num%.3f}),Map(items[1].num -> GetFn(items[1].num))))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,2,5), Item(xyz,1,7)),1)
                             |x -> Thingy = 7.000""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Length function work") {
      val script =
        """
          |  val x = items[].len()
          |  val y = "Foobar".len()
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,LengthFn(GetFn(items[]))), ValStmt(y,LengthFn(ConstantFn(Foobar)))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,2,5), Item(xyz,1,7)),1)
                             |x -> 2
                             |y -> 6""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == inst && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("'this' must work in map()") {
      val script =
        """
          |  items[].qty => this * 3 # this references qty
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(items[].qty,MultiplyFn(GetFn(this),ConstantFn(3)))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,6,5), Item(xyz,3,7)),1)""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa", List(Item("abc", 6, 5), Item("xyz", 3, 7)), 1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("distinct and limit must work") {
      val script =
        """
          |  items[].distinct(number).limit( 2 )
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(items[],PolyFn(List(DistinctFn(IdentityFn,Some(this.number)), LimitFn(IdentityFn,2))))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,2,5), Item(xyz,1,7)),1)""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("abc", 3, 44), Item("xyz", 1, 7), Item("foo", 1, 7), Item("bar", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7)), 1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("BiMap conversion must work") {
      val numbers = Map("abc" -> "p123", "xyz" -> "p456")
      val withRegistry = (new BiMapRegistry()).register("testmap", BiMap.fromMap(numbers))
      val inst = Shipment("aaa", List(Item("abc", 9, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]

      val scriptFwd =
        """
          |  items[].number.mapTo("testmap")
          |""".stripMargin
      val expectedCompiledFwd = """BlockStmt(List(MapStmt(items[].number,MapFwdFn(testmap))))"""
      val expectedResultFwd = """top -> Shipment(aaa,List(Item(p123,9,5), Item(p456,1,7)),1)""".stripMargin + "\n"

      val scriptRev =
        """
          |  items[].number.mapFrom("testmap")
          |""".stripMargin
      val expectedCompiledRev = """BlockStmt(List(MapStmt(items[].number,MapRevFn(testmap))))"""
      val expectedResultRev = """top -> Shipment(aaa,List(Item(abc,9,5), Item(xyz,1,7)),1)""".stripMargin + "\n"

      for {
        compiledScriptFwd <- Script.compile(scriptFwd, a)
        compiledScriptRev <- Script.compile(scriptRev, a)
        (f, newCtx) <- a.run(compiledScriptFwd, inst, withRegistry)
        resultStrFwd = toStringCtx(newCtx)
        (r, newnewCtx) <- a.run(compiledScriptRev, f, withRegistry)
        resultStrRev = toStringCtx(newnewCtx)
      } yield assertTrue(
        f == Shipment("aaa", List(Item("p123", 9, 5), Item("p456", 1, 7)), 1) && resultStrFwd == expectedResultFwd && compiledScriptFwd.toString == expectedCompiledFwd &&
          r == inst && resultStrRev == expectedResultRev && compiledScriptRev.toString == expectedCompiledRev
      )
    },
    test("No-ZIO must work") {
      val script =
        """
          |  items[].filter(this.qty > 4)
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(items[],FilterFn(IdentityFn,GreaterThanFn(GetFn(this.qty),ConstantFn(4))))))""".stripMargin
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,9,5)),1)""" + "\n"
      val inst = Shipment("aaa", List(Item("abc", 9, 5), Item("xyz", 1, 7)), 1)
      val a = dynalens[Shipment]

      val result: Either[DynaLensError, ((Shipment, DynaContext), BlockStmt)] =
        for {
          compiled <- Script.compileNoZIO(script, a)
          output <- a.runNoZIO(compiled, inst)
        } yield (output, compiled)

      result match {
        case Right(((updatedObj, ctx), comp)) =>
          val ctxStr = toStringCtx(ctx)
          assertTrue(updatedObj == Shipment("aaa", List(Item("abc", 9, 5)), 1))
          assertTrue(ctxStr == expectedResult)
          assertTrue(comp.toString == expectedCompiled)

        case Left(err: DynaLensError) =>
          assertTrue(false).label(s"DynaLens failed with error: ${err.msg}")
      }
    },
    test("Date and UUID functions must work") {
      val script =
        """
          |  id = uuid()
          |  val old = when.dateFmt("MM-dd-yy")
          |  val new = now().dateFmt("MM-dd-yyyy")
          |  when = "07-09-1972".toDate("MM-dd-yyyy")
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(UpdateStmt(id,UUIDFn()), ValStmt(old,FormatDateFn(GetFn(when),ConstantFn(MM-dd-yy))), ValStmt(new,FormatDateFn(NowFn(),ConstantFn(MM-dd-yyyy))), UpdateStmt(when,ParseDateFn(ConstantFn(07-09-1972),ConstantFn(MM-dd-yyyy)))))"""

      val inst = Ticket(java.util.UUID.randomUUID(), new java.util.Date())
      val a = dynalens[Ticket]

      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)

        // Extract actual values from ctx
        idVal <- a.get("id", x)
        oldVal = ctx("old")._1
        newVal = ctx("new")._1
        whenVal <- a.get("when", x)

        // Reformat parsed `when` back to check correctness
        formattedWhen = new java.text.SimpleDateFormat("yyyy-MM-dd").format(whenVal.asInstanceOf[java.util.Date])
      } yield assertTrue(
        compiledScript.toString == expectedCompiled,
        x.id != null,
        idVal.isInstanceOf[java.util.UUID],
        newVal.isInstanceOf[String],
        newVal.toString.matches("""\d{2}-\d{2}-\d{4}"""),
        oldVal.isInstanceOf[String],
        formattedWhen == "1972-07-09" // Confirm parsed date
      )
    },
    test("BiMap on simple type (String) must work") {
      val map = Map("a" -> "X", "b" -> "Y")
      val withRegistry = (new BiMapRegistry()).register("strMap", BiMap.fromMap(map))
      val inst = Registry("abc", Nil, List("a", "b"))
      val a = dynalens[Registry]

      val scriptFwd =
        """
          |  giftDesc[].mapTo("strMap")
          |""".stripMargin
      val expectedCompiledFwd = """BlockStmt(List(MapStmt(giftDesc[],MapFwdFn(strMap))))"""
      val expectedResultFwd = """top -> Registry(abc,List(),List(X, Y))""" + "\n"

      val scriptRev =
        """
          |  giftDesc[].mapFrom("strMap")
          |""".stripMargin
      val expectedCompiledRev = """BlockStmt(List(MapStmt(giftDesc[],MapRevFn(strMap))))"""
      val expectedResultRev = """top -> Registry(abc,List(),List(a, b))""" + "\n"

      for {
        compiledScriptFwd <- Script.compile(scriptFwd, a)
        compiledScriptRev <- Script.compile(scriptRev, a)
        (f, ctxF) <- a.run(compiledScriptFwd, inst, withRegistry)
        resultStrFwd = toStringCtx(ctxF)
        (r, ctxR) <- a.run(compiledScriptRev, f, withRegistry)
        resultStrRev = toStringCtx(ctxR)
      } yield assertTrue(
        f == Registry("abc", Nil, List("X", "Y")),
        resultStrFwd == expectedResultFwd,
        compiledScriptFwd.toString == expectedCompiledFwd,
        r == Registry("abc", Nil, List("a", "b")),
        resultStrRev == expectedResultRev,
        compiledScriptRev.toString == expectedCompiledRev
      )
    },
    test("No-ZIO simple value filtering must work (simple type)") {
      val script =
        """
          |  giftNums[].filter(this > 3)
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(giftNums[],FilterFn(IdentityFn,GreaterThanFn(GetFn(this),ConstantFn(3))))))"""
      val expectedResult = """top -> Registry(abc,List(5, 7),List())""" + "\n"
      val inst = Registry("abc", List(1, 5, 7), Nil)
      val a = dynalens[Registry]

      val result: Either[DynaLensError, ((Registry, DynaContext), BlockStmt)] =
        for {
          compiled <- Script.compileNoZIO(script, a)
          output <- a.runNoZIO(compiled, inst)
        } yield (output, compiled)

      result match {
        case Right(((updatedObj, ctx), comp)) =>
          val ctxStr = toStringCtx(ctx)
          assertTrue(updatedObj == Registry("abc", List(5, 7), Nil))
          assertTrue(ctxStr == expectedResult)
          assertTrue(comp.toString == expectedCompiled)
        case Left(err: DynaLensError) =>
          assertTrue(false).label(s"DynaLens failed with error: ${err.msg}")
      }
    },
    // corner case 1 is defined in the script
    test("corner case 1 must work") {
      val script =
        """
          |  items[].qty => items.len() * 2
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(items[].qty,MultiplyFn(LengthFn(GetFn(items[])),ConstantFn(2)))))"""
      val expectedResult = """top -> Shipment(aaa,List(Item(abc,10,5), Item(abc,10,44), Item(xyz,10,7), Item(foo,10,7), Item(bar,10,7)),1)""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("abc", 3, 44), Item("xyz", 1, 7), Item("foo", 1, 7), Item("bar", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa", List(Item("abc", 10, 5), Item("abc", 10, 44), Item("xyz", 10, 7), Item("foo", 10, 7), Item("bar", 10, 7)), 1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Map over list of primitives") {
      val script =
        """
          |  giftNums => 0
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(giftNums[],LoopFn(ConstantFn(0)))))"""
      val expectedResult = """top -> Registry(abc,List(0, 0, 0, 0, 0),List())""".stripMargin + "\n"
      val inst = Registry("abc", List(9, 2, 5, 6, 1), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Registry("abc", List(0, 0, 0, 0, 0), Nil) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Deep nesting with individual element") {
      val script =
        """
          |  pack.shipments[1].items.qty => 1
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(pack.shipments[1].items[].qty,ConstantFn(1))))"""
      val expectedResult = """top -> Order(ord1,Pack(palletA,4,List(Shipment(ship1,List(Item(a,5,7), Item(b,2,7), Item(c,100,7)),2), Shipment(ship2,List(Item(x,1,7), Item(y,1,7), Item(z,1,7)),2))))""".stripMargin + "\n"
      val inst =
        Order(
          "ord1",
          Pack(
            "palletA",
            4,
            List(
              Shipment("ship1", List(Item("a", 5), Item("b", 2), Item("c", 100))),
              Shipment("ship2", List(Item("x", 12), Item("y", 99), Item("z", 54)))
            )
          )
        )
      val a = dynalens[Order]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x ==         Order(
        "ord1",
        Pack(
          "palletA",
          4,
          List(
            Shipment("ship1", List(Item("a", 5), Item("b", 2), Item("c", 100))),
            Shipment("ship2", List(Item("x", 1), Item("y", 1), Item("z", 1)))
          )
        )
      ) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("Deep nesting with mutliple elements") {
      val script =
        """
          |  pack.shipments.items.number => this :: "." :: shipments.id
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(pack.shipments[].items[].number,ConcatFn(List(GetFn(this), ConstantFn(.), GetFn(shipments[].id))))))"""
      val expectedResult = """top -> Order(ord1,Pack(palletA,4,List(Shipment(ship1,List(Item(a.ship1,5,7), Item(b.ship1,2,7), Item(c.ship1,100,7)),2), Shipment(ship2,List(Item(x.ship2,12,7), Item(y.ship2,99,7), Item(z.ship2,54,7)),2))))""".stripMargin + "\n"
      val inst =
        Order(
          "ord1",
          Pack(
            "palletA",
            4,
            List(
              Shipment("ship1", List(Item("a", 5), Item("b", 2), Item("c", 100))),
              Shipment("ship2", List(Item("x", 12), Item("y", 99), Item("z", 54)))
            )
          )
        )
      val a = dynalens[Order]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Order(
        "ord1",
        Pack(
          "palletA",
          4,
          List(
            Shipment("ship1", List(Item("a.ship1", 5), Item("b.ship1", 2), Item("c.ship1", 100))),
            Shipment("ship2", List(Item("x.ship2", 12), Item("y.ship2", 99), Item("z.ship2", 54)))
          )
        )
      ) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    }
  )

//        _ <- ZIO.succeed(println("&&& " + compiledScript))
