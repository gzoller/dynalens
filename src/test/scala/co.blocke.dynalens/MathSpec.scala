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
import co.blocke.dynalens.MyLists
import co.blocke.dynalens.parser.Script
import zio.*
import zio.test.*

object MathSpec extends ZIOSpecDefault:

  def spec = suite("Math Functions Parsing Tests")(
    test("sum (assign to Int field)") {
      val script =
        """
          |  val s = l1.sum()
          |  id = s
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(s,SumFn(GetFn(l1[]))), UpdateStmt(id,GetFn(s))))"""
      val inst = MyLists(0, List(1, 2, 3, 4), None)
      val a = dynalens[MyLists]
      for {
        compiled <- Script.compile(script, a)
        (updated, ctx) <- a.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == MyLists(10, List(1, 2, 3, 4), None),
        compiled.toString == expectedCompiled
      ) &&
        assertTrue(
          // context should show s = 10 and top with id=10
          resultStr.contains("top -> MyLists(10,List(1, 2, 3, 4),None)") &&
            resultStr.contains("s -> 10")
        )
    },
    test("min (val only)") {
      val script =
        """
          |  val m = l1.min()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(m,MinFn(GetFn(l1[])))))"""
      val inst = MyLists(99, List(9, -2, 7, 4), None)
      val a = dynalens[MyLists]
      for {
        compiled <- Script.compile(script, a)
        (updated, ctx) <- a.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst,
        compiled.toString == expectedCompiled,
        resultStr.contains("m -> -2")
      )
    },
    test("max (chained with filter)") {
      val script =
        """
          |  val mx = l1.filter(this >= 3).max()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(mx,MaxFn(FilterFn(GetFn(l1[]),GreaterThanOrEqualFn(GetFn(this),ConstantFn(3)))))))"""
      val inst = MyLists(5, List(1, 3, 2, 10, 4), None)
      val a = dynalens[MyLists]
      for {
        compiled <- Script.compile(script, a)
        (updated, ctx) <- a.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst,
        compiled.toString == expectedCompiled,
        resultStr.contains("mx -> 10")
      )
    },
    test("avg (val only, returns Double)") {
      val script =
        """
          |  val a = l1.avg()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(a,AvgFn(GetFn(l1[])))))"""
      val inst = MyLists(0, List(2, 4, 6, 8), None) // avg = 5.0
      val aLens = dynalens[MyLists]
      for {
        compiled <- Script.compile(script, aLens)
        (updated, ctx) <- aLens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst,
        compiled.toString == expectedCompiled
      ) && assertTrue(
        resultStr.contains("a -> 5.0")
      )
    },
    test("median (odd length → Double of middle)") {
      val script =
        """
          |  val med = l1.median()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(med,MedianFn(GetFn(l1[])))))"""
      val inst = MyLists(0, List(9, 1, 7), None) // sorted: 1,7,9 → median = 7.0 or 7 depending on impl; ours is 7.0
      val a = dynalens[MyLists]
      for {
        compiled <- Script.compile(script, a)
        (updated, ctx) <- a.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst,
        compiled.toString == expectedCompiled,
        resultStr.contains("med -> 7.0")
      )
    },
    test("median (even length → average of two middles)") {
      val script =
        """
          |  val med = l1.median()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(med,MedianFn(GetFn(l1[])))))"""
      val inst = MyLists(0, List(1, 100, 2, 50), None) // sorted: 1,2,50,100 → (2+50)/2 = 26.0
      val a = dynalens[MyLists]
      for {
        compiled <- Script.compile(script, a)
        (updated, ctx) <- a.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst,
        compiled.toString == expectedCompiled,
        resultStr.contains("med -> 26.0")
      )
    },
    test("abs on scalar (assign back)") {
      val script =
        """
          |  val x = id.abs()
          |  id = x
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,AbsFn(GetFn(id))), UpdateStmt(id,GetFn(x))))"""
      val inst = MyLists(-12, List(1, 2, 3), None)
      val a = dynalens[MyLists]
      for {
        compiled <- Script.compile(script, a)
        (updated, ctx) <- a.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == MyLists(12, List(1, 2, 3), None),
        compiled.toString == expectedCompiled,
        resultStr.contains("x -> 12")
      )
    },
    test("sum on optional list when None → 0") {
      val script =
        """
          |  val s = l2.sum()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(s,SumFn(GetFn(l2[]?)))))"""
      val inst = MyLists(0, List(1, 2, 3), None) // l2 = None
      val a = dynalens[MyLists]
      for {
        compiled <- Script.compile(script, a)
        (updated, ctx) <- a.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst,
        compiled.toString == expectedCompiled,
        resultStr.contains("s -> 0")
      )
    },
    test("sum on optional list when Some(...)") {
      val script =
        """
          |  val s = l2.sum()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(s,SumFn(GetFn(l2[]?)))))"""
      val inst = MyLists(0, List(1, 2, 3), Some(List(10, 5)))
      val a = dynalens[MyLists]
      for {
        compiled <- Script.compile(script, a)
        (updated, ctx) <- a.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst,
        compiled.toString == expectedCompiled,
        resultStr.contains("s -> 15")
      )
    },
    test("chain: sort then median") {
      val script =
        """
          |  val med = l1.sortAsc().median()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(med,MedianFn(SortFn(GetFn(l1[]),None,true)))))"""
      val inst = MyLists(0, List(5, 1, 9, 3), None) // sorted: 1,3,5,9 → (3+5)/2 = 4.0
      val a = dynalens[MyLists]
      for {
        compiled <- Script.compile(script, a)
        (updated, ctx) <- a.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst,
        compiled.toString == expectedCompiled,
        resultStr.contains("med -> 4.0")
      )
    },
    test("chain: filter then sum") {
      val script =
        """
          |  val s = l1.filter(this % 2 == 0).sum()
          |""".stripMargin
      val expectedCompiled =
        """BlockStmt(List(ValStmt(s,SumFn(FilterFn(GetFn(l1[]),EqualFn(ModuloFn(GetFn(this),ConstantFn(2)),ConstantFn(0)))))))"""
      val inst = MyLists(0, List(1, 2, 3, 4, 5, 6), None) // evens: 2+4+6=12
      val a = dynalens[MyLists]
      for {
        compiled <- Script.compile(script, a)
        (updated, ctx) <- a.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst,
        compiled.toString == expectedCompiled,
        resultStr.contains("s -> 12")
      )
    }
  )
