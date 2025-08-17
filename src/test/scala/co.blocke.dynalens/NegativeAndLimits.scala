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

object NegativeAndLimits extends ZIOSpecDefault:

  def spec = suite("Negative and Limits Tests")(
    test("Minimal if-block as function result must work") {
      val script =
        """
          |  val flag = if (qty > 5) then {
          |    "big"
          |  } else {
          |    "small"
          |  }
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(flag,IfFn(GreaterThanFn(GetFn(qty,false),ConstantFn(5)),BlockFn(List(),ConstantFn(big)),BlockFn(List(),ConstantFn(small))))))"""

      val inst = Item("abc", 6)
      val a = dynalens[Item]

      for {
        compiled <- Script.compile(script, a)
        (_, ctx) <- a.run(compiled, inst)
      } yield assertTrue(
        ctx("flag")._1 == "big",
        compiled.toString == expectedCompiled
      )
    },
    test("Accessing nonexistent field should fail") {
      val script = "bogus = 99"
      val a = dynalens[Item]

      val result = Script.compileNoZIO(script, a).flatMap(compiled => a.runNoZIO(compiled, Item("abc", 2)))

      result match {
        case Left(err: DynaLensError) =>
          assertTrue(err.msg.contains("Error: Field 'bogus' does not exist in typeInfo"))
        case _ =>
          assertTrue(false).label("Expected error did not occur")
      }
    },
    test("Type mismatch: comparing string to int should fail") {
      val script = """val x = name > 5""" // name is String, not Int
      val a = dynalens[Person]

      val result = Script.compileNoZIO(script, a).flatMap(compiled => a.runNoZIO(compiled, Person("bob", 35)))
      result match {
        case Left(err: DynaLensError) =>
          assertTrue(err.msg.contains("Cannot compare types: class java.lang.String and class java.lang.Integer"))
        case _ =>
          assertTrue(false).label("Expected type mismatch error")
      }
    },
    test("mapTo with missing key should fail") {
      val bimap = BiMap.fromMap(Map("abc" -> "123"))
      val ctx = new BiMapRegistry().register("testmap", bimap)
      val inst = Registry("abc", Nil, List("xyz"))
      val a = dynalens[Registry]

      val script = """giftDesc[].mapTo("testmap")"""
      val result = for {
        compiled <- Script.compile(script, a)
        output <- a.run(compiled, inst, ctx)
      } yield output

      result.exit.map {
        case Exit.Failure(cause) =>
          cause.failureOption match
            case Some(DynaLensError(message)) =>
              assertTrue(message.contains("Key 'xyz' not found in forward map"))
            case _ =>
              assertTrue(false).label("Unexpected error structure")
        case _ =>
          assertTrue(false).label("Expected mapTo key failure")
      }
    },
    test("clean must remove null and empty strings") {
      val script = """giftDesc[].clean()"""
      val inst = Registry("r1", Nil, List("a", "", null, "b", ""))
      val a = dynalens[Registry]

      val expected = Registry("r1", Nil, List("a", "b"))
      val expectedCompiled = """BlockStmt(List(MapStmt(giftDesc[],CleanFn())))"""
      val expectedResult = "top -> Registry(r1,List(),List(a, b))\n"

      for {
        compiled <- Script.compile(script, a)
        (x, ctx) <- a.run(compiled, inst)
        ctxStr = toStringCtx(ctx)
      } yield assertTrue(
        x == expected,
        compiled.toString == expectedCompiled,
        ctxStr == expectedResult
      )
    },
    test("filter on primitive list using 'this' must work") {
      val script = """giftNums[].filter(this > 2)"""
      val inst = Registry("r1", List(1, 2, 3, 4), Nil)
      val a = dynalens[Registry]

      val expected = Registry("r1", List(3, 4), Nil)
      val expectedCompiled = """BlockStmt(List(MapStmt(giftNums[],FilterFn(GreaterThanFn(GetFn(this,true),ConstantFn(2))))))"""
      val expectedResult = "top -> Registry(r1,List(3, 4),List())\n"

      for {
        compiled <- Script.compile(script, a)
        (x, ctx) <- a.run(compiled, inst)
        ctxStr = toStringCtx(ctx)
      } yield assertTrue(
        x == expected,
        compiled.toString == expectedCompiled,
        ctxStr == expectedResult
      )
    },
    test("Assignment top optional list (empty)") {
      val script =
        """
          |  l2[2] = 99
          |""".stripMargin

      val inst = MyLists(1, List(1,2,3), None)
      val a    = dynalens[MyLists]
      val effect =
        Script.compile(script, a).flatMap(compiled => a.run(compiled, inst))

      for {
        res <- effect.either // Either[DynaLensError, (MyLists, DynaContext)]
      } yield res match {
        case Left(err) =>
          assertTrue(err.getMessage.contains("Index 2 out of bounds for field 'l2'"))
        case Right(_)  =>
          assertTrue(false).label("Expected a DynaLensError, but got success")
      }
    },
  )
