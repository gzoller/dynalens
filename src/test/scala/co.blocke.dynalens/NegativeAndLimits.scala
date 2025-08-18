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
    test("Unknown nested field should fail") {
      val script = "items[].bogus = 1"
      val a = dynalens[Shipment]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, /* your instance */ ???))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Field 'bogus' does not exist in typeInfo"))
        case _         => assertTrue(false).label("Expected unknown field error")
      }
    },
    test("Indexing a non-list should fail") {
      val script = "qty[0] = 1" // qty is an Int on Item
      val a = dynalens[Item]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Item("abc", 2)))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Cannot index into non-list field 'qty"))
        case _         => assertTrue(false).label("Expected 'not a Seq' error")
      }
    },
    test("Fixed index out of bounds should fail") {
      val script = "giftNums[10] = 9"
      val inst = Registry("r1", List(1,2,3), Nil)
      val a = dynalens[Registry]
      val eff = Script.compile(script, a).flatMap(c => a.run(c, inst))
      for (res <- eff.either) yield res match {
        case Left(err) => assertTrue(err.getMessage.contains("Index 10 out of bounds for field 'giftNums'"))
        case Right(_)  => assertTrue(false).label("Expected index OOB error")
      }
    },
    test("Unknown collection method should fail") {
      val script = "giftNums[].blorp()"
      val inst = Registry("r1", List(1,2,3), Nil)
      val a = dynalens[Registry]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, inst))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Unknown collection method: blorp"))
        case _         => assertTrue(false).label("Expected unknown method error")
      }
    },
    test("Non-boolean predicate in filter should fail") {
      val script = "giftNums[].filter(123)"
      val inst = Registry("r1", List(1,2,3), Nil)
      val a = dynalens[Registry]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, inst))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Expected Boolean result at runtime, but got: Integer = 123"))
        case _         => assertTrue(false).label("Expected boolean predicate error")
      }
    },
    test("'this' outside collection/map should fail") {
      val script = "qty = this * 2"
      val a = dynalens[Item]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Item("abc", 3)))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Field not found: this"))
        case _         => assertTrue(false).label("Expected 'this' misuse error")
      }
    },
//    test("Arrow '->' on non-optional LHS should fail") {
//      val script = "qty = -> (3)"
//      val a = dynalens[Item]
//      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Item("abc", 1)))
//      result match {
//        case Left(err) => assertTrue(err.msg.contains("'->' requires an optional scalar target"))
//        case _         => assertTrue(false).label("Expected arrow misuse error")
//      }
//    },
//    test("Optional scalar LHS with non-inferable RHS should fail") {
//      // dunno?: Option[String], RHS is a boolean fn here
//      val script = "dunno? = qty > 5"
//      val a = dynalens[Maybe] // case class Maybe(id: String, dunno: Option[String], ...)
//      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Maybe("id", None, None)))
//      result match {
//        case Left(err) => assertTrue(err.msg.contains("Unable to infer type of RHS"))
//        case _         => assertTrue(false).label("Expected RHS inference/type error")
//      }
//    },
//    test("Assigning None to non-optional field should fail") {
//      val script = "qty = None"
//      val a = dynalens[Item]
//      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Item("abc", 3)))
//      result match {
//        case Left(err) => assertTrue(err.msg.contains("type mismatch"))
//        case _         => assertTrue(false).label("Expected None→non-optional error")
//      }
//    }
    test("sortAsc with unknown key should fail") {
      val script = "pack.shipments.items.sortAsc(bogus)"
      val a = dynalens[Order] // whatever type has items[]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Order("a",Pack("b",1,List(Shipment("c",List(Item("d",3))))))))
      result match {
        case Left(err) =>
          assertTrue(err.msg.contains("Field 'bogus' does not exist"))
        case _         =>
          assertTrue(false).label("Expected unknown sort key error")
      }
    },
    test("Use of undeclared val symbol should fail") {
      val script = "qty = x + 1" // x never declared
      val a = dynalens[Item]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Item("abc", 2)))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Field 'x' does not exist"))
        case _         => assertTrue(false).label("Expected undeclared symbol error")
      }
    },

    /* Fix (needs to Instance, not Item()) and re-enable after we have function kinds
    test("equalsIgnoreCase on non-strings should fail") {
      val script = """val b = items.equalsIgnoreCase("x")"""
      val a = dynalens[Shipment]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Item("id", 7)))
      result match {
        case Left(err) => assertTrue(err.msg.contains("type mismatch"))
        case _         => assertTrue(false).label("Expected string-only method type error")
      }
    },
     */

    /*
    test("Method not allowed on map/object receiver should fail") {
      val script = "pack{}.filter(this > 0)" // if pack is an object/map-ish node
      val a = dynalens[Order]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, /* instance */ ???))
      result match {
        case Left(err) => assertTrue(err.msg.contains("not allowed on receiver"))
        case _         => assertTrue(false).label("Expected receiver kind error")
      }
    },
    */

    test("filter using unknown relative field should fail") {
      val script = "giftNums[].filter(qty > 2)" // giftNums is List[Int], no 'qty' in element scope
      val inst = Registry("r1", List(1,2,3), Nil)
      val a = dynalens[Registry]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, inst))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Field 'qty' does not exist"))
        case _         => assertTrue(false).label("Expected relative path field error")
      }
    },
  )
