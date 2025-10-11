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

import co.blocke.dynalens.CtxStrings.toStringCtx
import co.blocke.dynalens.DynaLens.dynalens
import co.blocke.dynalens.parser.Script
import co.blocke.dynalens.*
import zio.*
import zio.test.*

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
        """BlockStmt(List(ValStmt(flag,IfFn(GreaterThanFn(GetFn(qty,false,None),ConstantFn(5)),BlockFn(List(),ConstantFn(big)),BlockFn(List(),ConstantFn(small))))))"""
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
          assertTrue(err.msg.contains("Error: Field 'bogus' does not exist in schema, receiver, or symbol scope"))
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
          assertTrue(err.msg.contains("Error: > operands must be numeric (found java.lang.String, scala.Int)"))
        case _ =>
          assertTrue(false).label("Expected type mismatch error")
      }
    },
    test("mapTo with missing key should fail") {
      val bimap = BiMap.fromMap(Map("abc" -> "123"))
      val ctx = new BiMapRegistry().register("testmap", bimap)
      val inst = Registry("foo", Nil, List("xyz"))
      val a = dynalens[Registry]

      val script = """giftDesc.mapTo("testmap")"""
      val result = for {
        compiled <- Script.compile(script, a)
        _ <- ZIO.succeed(println(">>> "+compiled))
        output <- a.run(compiled, inst, ctx)
        _ <- ZIO.succeed(println("!!! "+output))
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
      val script = """giftDesc.clean()"""
      val inst = Registry("r1", Nil, List("a", "", null, "b", ""))
      val a = dynalens[Registry]

      val expected = Registry("r1", Nil, List("a", "b"))
      val expectedCompiled = """BlockStmt(List(MapStmt(giftDesc,CleanFn(GetFn(giftDesc,false,None)))))"""
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
      val script = """giftNums.filter(this > 2)"""
      val inst = Registry("r1", List(1, 2, 3, 4), Nil)
      val a = dynalens[Registry]

      val expected = Registry("r1", List(3, 4), Nil)
      val expectedCompiled = """BlockStmt(List(MapStmt(giftNums,FilterFn(GetFn(giftNums,false,None),GreaterThanFn(GetFn(this,false,Some(GetFn(giftNums,false,None))),ConstantFn(2))))))"""
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
    test("Assignment to optional list (empty)") {
      val script =
        """
          |  l2[2] = 99
          |""".stripMargin
      val inst = MyLists(1, List(1, 2, 3), None)
      val a = dynalens[MyLists]
      val expectedCompiled = """BlockStmt(List(UpdateStmt(l2[2],ConstantFn(99))))"""
      val expectedResult = "top -> MyLists(1,List(1, 2, 3),None)\n"

      for {
        compiled <- Script.compile(script, a)
        (x, ctx) <- a.run(compiled, inst)
        ctxStr = toStringCtx(ctx)
      } yield assertTrue(
        x == inst,
        compiled.toString == expectedCompiled,
        ctxStr == expectedResult
      )
    },
    test("Assignment to optional list (non-empty)") {
      val script =
        """
          |  l2[2] = 99
          |""".stripMargin
      val inst = MyLists(1, List(1, 2, 3), Some(List(4,5,6)))
      val a = dynalens[MyLists]
      val expectedCompiled = """BlockStmt(List(UpdateStmt(l2[2],ConstantFn(99))))"""
      val expectedResult = "top -> MyLists(1,List(1, 2, 3),Some(List(4, 5, 99)))\n"

      for {
        compiled <- Script.compile(script, a)
        (x, ctx) <- a.run(compiled, inst)
        ctxStr = toStringCtx(ctx)
      } yield assertTrue(
        x == MyLists(1, List(1, 2, 3), Some(List(4,5,99))),
        compiled.toString == expectedCompiled,
        ctxStr == expectedResult
      )
    },
    test("Assignment to optional list (non-empty--out of bounds)") {
      val script =
        """
          |  l2[3] = 99
          |""".stripMargin
      val inst = MyLists(1, List(1, 2, 3), Some(List(4,5,6)))
      val a = dynalens[MyLists]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, inst))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Index 3 out of bounds for field 'l2'"))
        case _         => assertTrue(false).label("Expected unknown field error")
      }
    },
    test("Unknown nested field should fail") {
      val script = "items.bogus = 1"
      val a = dynalens[Shipment]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, /* your instance */ ???))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Error: Field 'bogus' does not exist here"))
        case _         => assertTrue(false).label("Expected unknown field error")
      }
    },
    test("Indexing a non-list should fail (assignment)") {
      val script = "qty[0] = 1" // qty is an Int on Item
      val a = dynalens[Item]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Item("abc", 2)))
      println(">>> "+Script.compileNoZIO(script, a))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Cannot index into non-list field 'qty"))
        case _         => assertTrue(false).label("Expected 'not a Seq' error")
      }
    },
    test("Indexing a non-list should fail (path)") {
      val script = """val x = if qty[3] > 2 then 1 else 0""" // qty is Int
      val a = dynalens[Item]
      val result = Script.compileNoZIO(script, a)
      result match {
        case Left(err: DynaLensError) =>
          assertTrue(err.msg.contains("Cannot index into non-list field 'qty'"))
        case Right(_) =>
          assertTrue(false).label("Expected compile-time path validation error for qty[3]")
      }
    },
    test("Fixed index out of bounds should fail") {
      val script = "giftNums[10] = 9"
      val inst = Registry("r1", List(1, 2, 3), Nil)
      val a = dynalens[Registry]
      val eff = Script.compile(script, a).flatMap(c => a.run(c, inst))
      for (res <- eff.either) yield res match {
        case Left(err) => assertTrue(err.getMessage.contains("Index 10 out of bounds for field 'giftNums'"))
        case Right(_)  => assertTrue(false).label("Expected index OOB error")
      }
    },
    test("Unknown collection method should fail") {
      val script = "giftNums.blorp()"
      val inst = Registry("r1", List(1, 2, 3), Nil)
      val a = dynalens[Registry]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, inst))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Unknown method: blorp"))
        case _         => assertTrue(false).label("Expected unknown method error")
      }
    },
    test("Non-boolean predicate in filter should fail") {
      val script = "giftNums.filter(123)"
      val inst = Registry("r1", List(1, 2, 3), Nil)
      val a = dynalens[Registry]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, inst))
      result match {
        case Left(err) => assertTrue(err.msg.contains(" Error: filter() requires boolean predicate, got scala.Int"))
        case _         => assertTrue(false).label("Expected boolean predicate error")
      }
    },
    test("'this' outside collection/map should fail (assignment)") {
      val script = "qty = this * 2"
      val a = dynalens[Item]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Item("abc", 3)))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Use of 'this' with no receiver in scope"))
        case _         => assertTrue(false).label("Expected 'this' misuse error")
      }
    },
    test("'this' outside collection/map should fail (valdef)") {
      val script = "val x = this * 2"
      val a = dynalens[Item]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Item("abc", 3)))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Use of 'this' with no receiver in scope"))
        case _         => assertTrue(false).label("Expected 'this' misuse error")
      }
    },
    test("Optional scalar LHS with non-inferable RHS should fail") {
      // dunno?: Option[String], RHS is a boolean fn here
      val script = "dunno = 3 > 5"
      val a = dynalens[Maybe] // case class Maybe(id: String, dunno: Option[String], ...)
      println(">>> "+Script.compileNoZIO(script, a))
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Maybe("id", None, None)))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Error: Type mismatch: cannot assign scala.Boolean to Option[java.lang.String] at dunno"))
        case _         => assertTrue(false).label("Expected RHS inference/type error")
      }
    },
    test("Assigning None to non-optional field should fail") {
      val script = "qty = None"
      val a = dynalens[Item]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Item("abc", 3)))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Error: Type mismatch: cannot assign Option[scala.Any] to scala.Int at qty"))
        case _         => assertTrue(false).label("Expected None→non-optional error")
      }
    },
    test("sortAsc with unknown key should fail") {
      val script = "pack.shipments.items.sortAsc(bogus)"
      val a = dynalens[Order] // whatever type has items[]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Order("a", Pack("b", 1, List(Shipment("c", List(Item("d", 3))))))))
      result match {
        case Left(err) =>
          assertTrue(err.msg.contains("Field 'bogus' does not exist"))
        case _ =>
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
    test("equalsIgnoreCase on non-strings should fail") {
      val script = """val b = items.equalsIgnoreCase("x")"""
      val a = dynalens[Shipment]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, Shipment("aaa", List(Item("wow", 9, 5), Item("xyz", 1, 7), Item("abc", 19, 7)), 1)))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Error: Method 'equalsIgnoreCase' cannot be applied to receiver of type scala.collection.immutable.List[co.blocke.dynalens.Item]"))
        case _         => assertTrue(false).label("Expected string-only method type error")
      }
    },
    test("Method not allowed on map/object receiver should fail") {
      val script = "pack.filter(this > 0)" // if pack is an object/map-ish node
      val a = dynalens[Order]
      val result = Script
        .compileNoZIO(script, a)
        .flatMap(c =>
          a.runNoZIO(
            c,
            Order(
              "ord1",
              Pack(
                "pallet",
                2,
                List(
                  Shipment("aaa", List(Item("wow", 9, 5), Item("xyz", 1, 7), Item("abc", 19, 7)), 1),
                  Shipment("bbb", List(Item("free", 7, 5), Item("ace", 5, 7), Item("xyz", 1, 7)), 1)
                )
              )
            )
          )
        )
      result match {
        case Left(err) => assertTrue(err.msg.contains("Error: > operands must be numeric (found co.blocke.dynalens.Pack, scala.Int)"))
        case _         => assertTrue(false).label("Expected receiver kind error")
      }
    },
    test("filter using unknown relative field should fail") {
      val script = "giftNums.filter(qty > 2)" // giftNums is List[Int], no 'qty' in element scope
      val inst = Registry("r1", List(1, 2, 3), Nil)
      val a = dynalens[Registry]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, inst))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Field 'qty' does not exist"))
        case _         => assertTrue(false).label("Expected relative path field error")
      }
    },
    test("invalid list method chains should fail") {
      val script =
        """
          |  val x = l1.distinct()
          |  val y = x[2].sortAsc()
          |""".stripMargin
      val inst = ComplexLists(1, List(1, 5, 8, 1, 0, 99, -2), Nil)
      val a = dynalens[ComplexLists]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, inst))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Error: Method 'sortAsc' cannot be applied to receiver of type scala.Int"))
        case _         => assertTrue(false).label("Expected relative path field error")
      }
    },
    test("indexed list methods type mismatch must fail") {
      val script =
        """
          |  val x = cplx.values()[0] + 2
          |""".stripMargin
      val inst = Mapped(1, Map.empty, None, Map("a" -> List(Person("Sam", 45))))
      val a = dynalens[Mapped]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, inst))
      result match {
        case Left(err) => assertTrue(err.msg.contains("Error: + requires numeric operands, found scala.collection.immutable.List[co.blocke.dynalens.Person] and scala.Int"))
        case _         => assertTrue(false).label("Expected relative path field error")
      }
    },
    test("map get (simple) fails on bad key for m") {
      val script =
        """
          |  val x = m.get("zzz")
          |""".stripMargin
      val inst = Mapped(
        id = 1,
        m = Map("a" -> 10, "b" -> 20),
        om = Some(Map("a" -> 5)),
        cplx = Map("a" -> List(Person("p", 1)))
      )
      val a = dynalens[Mapped]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, inst))
      result match {
        case Left(err) => assertTrue(err.msg.contains("get(): key 'zzz' not found"))
        case _         => assertTrue(false).label("Expected relative path field error")
      }
    },
    test("map get (optional map) fails on bad key for om") {
      val script =
        """
          |  val x = om.get("zzz")
          |""".stripMargin
      val inst = Mapped(
        id = 1,
        m = Map("a" -> 10, "b" -> 20),
        om = Some(Map("a" -> 5)),
        cplx = Map("a" -> List(Person("p", 1)))
      )
      val a = dynalens[Mapped]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, inst))
      result match {
        case Left(err) => assertTrue(err.msg.contains("get(): key 'zzz' not found"))
        case _         => assertTrue(false).label("Expected relative path field error")
      }
    },
    test("No case match (strict)") {
      val script =
        """
          |  m => ( this.key
          |       , this.value case {
          |           1  -> 7
          |           2 -> 19
          |         })
          |""".stripMargin
      val inst = Mapped(1, m = Map("a" -> 1, "b" -> 2, "c" -> 3), om = None, cplx = Map())
      val a = dynalens[Mapped]
      val result = Script.compileNoZIO(script, a).flatMap(c => a.runNoZIO(c, inst))
      result match {
        case Left(err) => assertTrue(err.msg.contains("No case matched for value: 3"))
        case _         => assertTrue(false).label("Expected relative path field error")
      }
    }
  )
