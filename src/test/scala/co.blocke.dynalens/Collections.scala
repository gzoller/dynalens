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

import co.blocke.dynalens.CtxStrings.toStringCtx
import co.blocke.dynalens.DynaLens.dynalens
import co.blocke.dynalens.parser.Script
import co.blocke.dynalens.*
import zio.*
import zio.test.*

object Collections extends ZIOSpecDefault:

  def spec = suite("Collections Parsing Tests")(
    test("filter (chain--simple)") {
      val script =
        """
          |  val x = giftNums.filter(this < 6)
          |  giftNums = x
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,FilterFn(GetFn(giftNums[]),LessThanFn(GetFn(this),ConstantFn(6)))), UpdateStmt(giftNums[],GetFn(x))))"""
      val expectedResult =
        """top -> Registry(abc,List(2, 5, 5, 2),List())
          |x -> List(2, 5, 5, 2)""".stripMargin + "\n"
      val inst = Registry("abc", List(2, 5, 7, 5, 2, 9), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Registry("abc", List(2, 5, 5, 2), List()) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("filter (map--simple)") {
      val script =
        """
          |  giftNums.filter(this<6)
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(giftNums[],FilterFn(IdentityFn,LessThanFn(GetFn(this),ConstantFn(6))))))"""
      val expectedResult =
        """top -> Registry(abc,List(2, 5, 5, 2),List())""".stripMargin + "\n"
      val inst = Registry("abc", List(2, 5, 7, 5, 2, 9), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Registry("abc", List(2, 5, 5, 2), List()) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("filter (chain--class)") {
      val script =
        """
          |  val x = items.filter(qty >= 100)
          |  val y = items.filter(qty < 0)
          |  items = x
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,FilterFn(GetFn(items[]),GreaterThanOrEqualFn(GetFn(this.qty),ConstantFn(100)))), ValStmt(y,FilterFn(GetFn(items[]),LessThanFn(GetFn(this.qty),ConstantFn(0)))), UpdateStmt(items[],GetFn(x))))"""
      val expectedResult =
        """top -> Shipment(aaa,List(Item(abc,100,5), Item(xyz,101,7)),1)
          |x -> List(Item(abc,100,5), Item(xyz,101,7))
          |y -> List()""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 100, 5), Item("qrs", 2, 5), Item("xyz", 101, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa", List(Item("abc", 100, 5), Item("xyz", 101, 7)), 1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("filter (map--class)") {
      val script =
        """
          |  items.filter(qty >= 100)
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(items[],FilterFn(IdentityFn,GreaterThanOrEqualFn(GetFn(this.qty),ConstantFn(100))))))"""
      val expectedResult =
        """top -> Shipment(aaa,List(Item(abc,100,5), Item(xyz,101,7)),1)""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 100, 5), Item("qrs", 2, 5), Item("xyz", 101, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, newCtx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(newCtx)
      } yield assertTrue(x == Shipment("aaa", List(Item("abc", 100, 5), Item("xyz", 101, 7)), 1) && resultStr == expectedResult && compiledScript.toString == expectedCompiled)
    },
    test("distinct (chain--simple)") {
      val script =
        """
          |  val x = giftNums.distinct()
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,DistinctFn(GetFn(giftNums[]),None))))"""
      val expectedResult =
        """top -> Registry(abc,List(2, 5, 7, 5, 2, 9),List())
          |x -> List(2, 5, 7, 9)""".stripMargin + "\n"
      val inst = Registry("abc", List(2, 5, 7, 5, 2, 9), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("distinct (map--simple)") {
      val script =
        """
          |  giftNums[].distinct()
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(giftNums[],DistinctFn(IdentityFn,None))))"""
      val expectedResult = """top -> Registry(abc,List(2, 5, 7, 9),List())""" + "\n"
      val inst = Registry("abc", List(2, 5, 7, 5, 2, 9), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == Registry("abc", List(2, 5, 7, 9), Nil),
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("distinct (chain--class)") {
      val script =
        """
          |  val x = items.distinct(num)
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,DistinctFn(GetFn(items[]),Some(this.num)))))"""
      val expectedResult =
        """top -> Shipment(aaa,List(Item(abc,2,5), Item(abc,3,44), Item(xyz,1,7), Item(foo,1,7), Item(bar,1,7)),1)
          |x -> List(Item(abc,2,5), Item(abc,3,44), Item(xyz,1,7))""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("abc", 3, 44), Item("xyz", 1, 7), Item("foo", 1, 7), Item("bar", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("distinct (map--class)") {
      val script =
        """
          |  items.distinct(num)
          |  val z = items.distinct(number) # bonus
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(items[],DistinctFn(IdentityFn,Some(this.num))), ValStmt(z,DistinctFn(GetFn(items[]),Some(this.number)))))"""
      val expectedResult =
        """top -> Shipment(aaa,List(Item(abc,2,5), Item(abc,3,44), Item(xyz,1,7)),1)
          |z -> List(Item(abc,2,5), Item(xyz,1,7))""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), Item("abc", 3, 44), Item("xyz", 1, 7), Item("foo", 1, 7), Item("bar", 1, 7)), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == Shipment("aaa", List(Item("abc", 2, 5), Item("abc", 3, 44), Item("xyz", 1, 7)), 1),
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("clean (chain--simple)") {
      val script =
        """
          |  val x = giftNums.clean()
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,CleanFn(GetFn(giftNums[])))))"""
      val expectedResult =
        """top -> Registry(abc,List(2, 5, 7, 5, 2, 9),List())
          |x -> List(2, 5, 7, 5, 2, 9)""".stripMargin + "\n"
      val inst = Registry("abc", List(2, 5, 7, 5, 2, 9), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("clean (map--simple)") {
      val script =
        """
          |  giftNums[].clean()
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(giftNums[],CleanFn(IdentityFn))))"""
      val expectedResult = """top -> Registry(abc,List(2, 5, 7, 5, 2, 9),List())""" + "\n"
      val inst = Registry("abc", List(2, 5, 7, 5, 2, 9), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == Registry("abc", List(2, 5, 7, 5, 2, 9), Nil),
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("clean (chain--class)") {
      val script =
        """
          |  val x = items.clean()
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,CleanFn(GetFn(items[])))))"""
      val expectedResult =
        """top -> Shipment(aaa,List(Item(abc,2,5), null, Item(xyz,1,7), Item(foo,1,7), null),1)
          |x -> List(Item(abc,2,5), Item(xyz,1,7), Item(foo,1,7))""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), null, Item("xyz", 1, 7), Item("foo", 1, 7), null), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("clean (map--class)") {
      val script =
        """
          |  items.clean()
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(items[],CleanFn(IdentityFn))))"""
      val expectedResult =
        """top -> Shipment(aaa,List(Item(abc,2,5), Item(xyz,1,7), Item(foo,1,7)),1)""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), null, Item("xyz", 1, 7), Item("foo", 1, 7), null), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == Shipment("aaa", List(Item("abc", 2, 5), Item("xyz", 1, 7), Item("foo", 1, 7)), 1),
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("reverse (chain--simple)") {
      val script =
        """
          |  val x = giftNums.reverse()
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,ReverseFn(GetFn(giftNums[])))))"""
      val expectedResult =
        """top -> Registry(abc,List(2, 5, 7, 5, 2, 9),List())
          |x -> List(9, 2, 5, 7, 5, 2)""".stripMargin + "\n"
      val inst = Registry("abc", List(2, 5, 7, 5, 2, 9), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("reverse (map--simple)") {
      val script =
        """
          |  giftNums[].reverse()
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(giftNums[],ReverseFn(IdentityFn))))"""
      val expectedResult = """top -> Registry(abc,List(9, 2, 5, 7, 5, 2),List())""" + "\n"
      val inst = Registry("abc", List(2, 5, 7, 5, 2, 9), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == Registry("abc", List(9, 2, 5, 7, 5, 2), Nil),
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("reverse (chain--class)") {
      val script =
        """
          |  val x = items.reverse()
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,ReverseFn(GetFn(items[])))))"""
      val expectedResult =
        """top -> Shipment(aaa,List(Item(abc,2,5), null, Item(xyz,1,7), Item(foo,1,7), null),1)
          |x -> List(null, Item(foo,1,7), Item(xyz,1,7), null, Item(abc,2,5))""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), null, Item("xyz", 1, 7), Item("foo", 1, 7), null), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("reverse (map--class)") {
      val script =
        """
          |  items.reverse()
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(items[],ReverseFn(IdentityFn))))"""
      val expectedResult =
        """top -> Shipment(aaa,List(null, Item(foo,1,7), Item(xyz,1,7), null, Item(abc,2,5)),1)""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), null, Item("xyz", 1, 7), Item("foo", 1, 7), null), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == Shipment("aaa", List(null, Item("foo", 1, 7), Item("xyz", 1, 7), null, Item("abc", 2, 5)), 1),
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("limit (chain--simple)") {
      val script =
        """
          |  val x = giftNums.limit(3)
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,LimitFn(GetFn(giftNums[]),3))))"""
      val expectedResult =
        """top -> Registry(abc,List(2, 5, 7, 5, 2, 9),List())
          |x -> List(2, 5, 7)""".stripMargin + "\n"
      val inst = Registry("abc", List(2, 5, 7, 5, 2, 9), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("limit (map--simple)") {
      val script =
        """
          |  giftNums[].limit(3)
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(giftNums[],LimitFn(IdentityFn,3))))"""
      val expectedResult = """top -> Registry(abc,List(2, 5, 7),List())""" + "\n"
      val inst = Registry("abc", List(2, 5, 7, 5, 2, 9), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == Registry("abc", List(2, 5, 7), Nil),
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("limit (chain--class)") {
      val script =
        """
          |  val x = items.limit(3)
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(ValStmt(x,LimitFn(GetFn(items[]),3))))"""
      val expectedResult =
        """top -> Shipment(aaa,List(Item(abc,2,5), null, Item(xyz,1,7), Item(foo,1,7), null),1)
          |x -> List(Item(abc,2,5), null, Item(xyz,1,7))""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), null, Item("xyz", 1, 7), Item("foo", 1, 7), null), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("limit (map--class)") {
      val script =
        """
          |  items.limit(3)
          |""".stripMargin
      val expectedCompiled = """BlockStmt(List(MapStmt(items[],LimitFn(IdentityFn,3))))"""
      val expectedResult =
        """top -> Shipment(aaa,List(Item(abc,2,5), null, Item(xyz,1,7)),1)""".stripMargin + "\n"
      val inst = Shipment("aaa", List(Item("abc", 2, 5), null, Item("xyz", 1, 7), Item("foo", 1, 7), null), 1)
      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == Shipment("aaa", List(Item("abc", 2, 5), null, Item("xyz", 1, 7)), 1),
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("sortAsc (chain--simple)") {
      val script =
        """
          |  val x = giftNums.sortAsc()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,SortFn(GetFn(giftNums[]),None,true))))"""
      val expectedResult =
        """top -> Registry(abc,List(2, 5, 7, 5, 2, 9),List())
          |x -> List(2, 2, 5, 5, 7, 9)""".stripMargin + "\n"

      val inst = Registry("abc", List(2, 5, 7, 5, 2, 9), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("sortAsc (map--simple)") {
      val script =
        """
          |  giftNums[].sortAsc()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(MapStmt(giftNums[],SortFn(IdentityFn,None,true))))"""
      val expectedResult =
        """top -> Registry(abc,List(2, 2, 5, 5, 7, 9),List())""" + "\n"

      val inst = Registry("abc", List(2, 5, 7, 5, 2, 9), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == Registry("abc", List(2, 2, 5, 5, 7, 9), Nil),
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("sortAsc (chain--class by field)") {
      val script =
        """
          |  val x = items.sortAsc(num)
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,SortFn(GetFn(items[]),Some(this.num),true))))"""

      // Distinct 'num' values for deterministic ordering
      val items0 = List(
        Item("e", 9, 9),
        Item("a", 1, 1),
        Item("c", 5, 5),
        Item("b", 3, 3),
        Item("d", 7, 7)
      )
      val inst = Shipment("aaa", items0, 1)
      val expectedX =
        List(
          Item("a", 1, 1),
          Item("b", 3, 3),
          Item("c", 5, 5),
          Item("d", 7, 7),
          Item("e", 9, 9)
        )
      val expectedResult =
        s"""top -> ${inst.toString}
           |x -> ${expectedX.toString}""".stripMargin + "\n"

      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("sortAsc (map--class by field)") {
      val script =
        """
          |  items.sortAsc(num)
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(MapStmt(items[],SortFn(IdentityFn,Some(this.num),true))))"""

      val items0 = List(
        Item("e", 9, 9),
        Item("a", 1, 1),
        Item("c", 5, 5),
        Item("b", 3, 3),
        Item("d", 7, 7)
      )
      val inst = Shipment("aaa", items0, 1)
      val updated =
        Shipment(
          "aaa",
          List(
            Item("a", 1, 1),
            Item("b", 3, 3),
            Item("c", 5, 5),
            Item("d", 7, 7),
            Item("e", 9, 9)
          ),
          1
        )
      val expectedResult =
        s"""top -> ${updated.toString}""" + "\n"

      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == updated,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("sortDesc (chain--simple)") {
      val script =
        """
          |  val x = giftNums.sortDesc()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,SortFn(GetFn(giftNums[]),None,false))))"""
      val expectedResult =
        """top -> Registry(abc,List(2, 5, 7, 5, 2, 9),List())
          |x -> List(9, 7, 5, 5, 2, 2)""".stripMargin + "\n"

      val inst = Registry("abc", List(2, 5, 7, 5, 2, 9), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("sortDesc (map--simple)") {
      val script =
        """
          |  giftNums[].sortDesc()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(MapStmt(giftNums[],SortFn(IdentityFn,None,false))))"""
      val expectedResult =
        """top -> Registry(abc,List(9, 7, 5, 5, 2, 2),List())""" + "\n"

      val inst = Registry("abc", List(2, 5, 7, 5, 2, 9), Nil)
      val a = dynalens[Registry]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == Registry("abc", List(9, 7, 5, 5, 2, 2), Nil),
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("sortDesc (chain--class by field)") {
      val script =
        """
          |  val x = items.sortDesc(num)
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,SortFn(GetFn(items[]),Some(this.num),false))))"""

      val items0 = List(
        Item("e", 9, 9),
        Item("a", 1, 1),
        Item("c", 5, 5),
        Item("b", 3, 3),
        Item("d", 7, 7)
      )
      val inst = Shipment("aaa", items0, 1)
      val expectedX =
        List(
          Item("e", 9, 9),
          Item("d", 7, 7),
          Item("c", 5, 5),
          Item("b", 3, 3),
          Item("a", 1, 1)
        )
      val expectedResult =
        s"""top -> ${inst.toString}
           |x -> ${expectedX.toString}""".stripMargin + "\n"

      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("sortDesc (map--class by field)") {
      val script =
        """
          |  items.sortDesc(num)
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(MapStmt(items[],SortFn(IdentityFn,Some(this.num),false))))"""

      val items0 = List(
        Item("e", 9, 9),
        Item("a", 1, 1),
        Item("c", 5, 5),
        Item("b", 3, 3),
        Item("d", 7, 7)
      )
      val inst = Shipment("aaa", items0, 1)
      val updated =
        Shipment(
          "aaa",
          List(
            Item("e", 9, 9),
            Item("d", 7, 7),
            Item("c", 5, 5),
            Item("b", 3, 3),
            Item("a", 1, 1)
          ),
          1
        )
      val expectedResult =
        s"""top -> ${updated.toString}""" + "\n"

      val a = dynalens[Shipment]
      for {
        compiledScript <- Script.compile(script, a)
        (x, ctx) <- a.run(compiledScript, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        x == updated,
        resultStr == expectedResult,
        compiledScript.toString == expectedCompiled
      )
    },
    test("map get (simple) succeeds on m") {
      val script =
        """
          |  val x = m.get("a")
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,MapGetFn(GetFn(m),ConstantFn(a)))))"""
      val expectedResult =
        """top -> Mapped(1,Map(a -> 10, b -> 20),Some(Map(a -> 5)),Map(a -> List(Person(p,1))))
          |x -> 10""".stripMargin + "\n"
      val inst = Mapped(
        id = 1,
        m = Map("a" -> 10, "b" -> 20),
        om = Some(Map("a" -> 5)),
        cplx = Map("a" -> List(Person("p", 1)))
      )
      val lens = dynalens[Mapped]
      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst, // no mutation
        compiled.toString == expectedCompiled,
        resultStr == expectedResult
      )
    },
    test("map get (optional map) succeeds on om") {
      val script =
        """
          |  val x = om.get("a")
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,MapGetFn(GetFn(om?),ConstantFn(a)))))"""
      val expectedResult =
        """top -> Mapped(1,Map(a -> 10, b -> 20),Some(Map(a -> 5)),Map(a -> List(Person(p,1))))
          |x -> 5""".stripMargin + "\n"
      val inst = Mapped(
        id = 1,
        m = Map("a" -> 10, "b" -> 20),
        om = Some(Map("a" -> 5)),
        cplx = Map("a" -> List(Person("p", 1)))
      )
      val lens = dynalens[Mapped]
      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst,
        compiled.toString == expectedCompiled,
        resultStr == expectedResult
      )
    },
    test("map get (complex value) + chain len() on cplx") {
      val script =
        """
          |  val x = cplx.get("a").len()
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,LengthFn(MapGetFn(GetFn(cplx),ConstantFn(a))))))"""

      val expectedResult =
        """top -> Mapped(1,Map(a -> 10, b -> 20),Some(Map(a -> 5)),Map(a -> List(Person(p,1))))
          |x -> 1""".stripMargin + "\n"

      val inst = Mapped(
        id = 1,
        m = Map("a" -> 10, "b" -> 20),
        om = Some(Map("a" -> 5)),
        cplx = Map("a" -> List(Person("p", 1)))
      )
      val lens = dynalens[Mapped]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst,
        compiled.toString == expectedCompiled,
        resultStr == expectedResult
      )
    },
    test("map map (identity)") {
      val script =
        """
          |  m => (this.key, this.value)
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(MapStmt(m,Tuple2Fn(GetFn(this.key),GetFn(this.value)))))"""
      val expectedResult =
        """top -> Mapped(1,Map(a -> 10, b -> 20),None,Map())""".stripMargin + "\n"
      val inst = Mapped(
        id = 1,
        m = Map("a" -> 10, "b" -> 20),
        None,
        Map.empty
      )
      val a = dynalens[Mapped]
      for {
        compiled <- Script.compile(script, a)
        (updated, ctx) <- a.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst, // identity
        compiled.toString == expectedCompiled,
        resultStr == expectedResult
      )
    },
    test("map map (value transform inline)") {
      val script =
        """
          |  m => (this.key, this.value * 3)
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(MapStmt(m,Tuple2Fn(GetFn(this.key),MultiplyFn(GetFn(this.value),ConstantFn(3))))))"""
      val expectedResult =
        """top -> Mapped(1,Map(a -> 30, b -> 60),None,Map())""".stripMargin + "\n"
      val inst = Mapped(
        id = 1,
        m = Map("a" -> 10, "b" -> 20),
        None,
        Map.empty
      )
      val lens = dynalens[Mapped]
      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == Mapped(1, Map("a" -> 30, "b" -> 60), None, Map.empty),
        compiled.toString == expectedCompiled,
        resultStr == expectedResult
      )
    },
    test("map map (block body with val)") {
      val script =
        """
          |  m => {
          |    val x = this.value * 3
          |    (this.key, x)
          |  }
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(MapStmt(m,BlockFn(List(ValStmt(x,MultiplyFn(GetFn(this.value),ConstantFn(3)))),Tuple2Fn(GetFn(this.key),GetFn(x))))))"""

      val expectedResult =
        """top -> Mapped(1,Map(a -> 30, b -> 60),None,Map())""".stripMargin + "\n"

      val inst = Mapped(
        id = 1,
        m = Map("a" -> 10, "b" -> 20),
        None,
        Map.empty
      )
      val lens = dynalens[Mapped]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == Mapped(1, Map("a" -> 30, "b" -> 60), None, Map.empty),
        compiled.toString == expectedCompiled,
        resultStr == expectedResult
      )
    }
  )

//  _ <- ZIO.succeed(println("&&& " + compiledScript))
//  _ <- ZIO.succeed(println("??? " + resultStr))
