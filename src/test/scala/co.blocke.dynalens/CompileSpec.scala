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

import zio._
import zio.test._
import zio.test.Assertion._

import DynaLens.*
import parser.Script

object CompileSpec extends ZIOSpecDefault {

  private def normalize(s: String): String =
    s.replaceAll("\\s+", "")

  def spec: Spec[Any, Throwable] = suite("CompileSpec – type checking and AST verification")(

    // ------------------------------------------------------------------
    // Positive tests – must compile and match the expected AST
    // ------------------------------------------------------------------

    test("arithmetic precedence with indexed list element") {
      val script =
        """val x = 3 + items[1].num * 2
          |val y = (3 + items[1].num) * 2
          |""".stripMargin

      val expected =
        """BlockStmt(List(
          |  ValStmt(x,AddFn(ConstantFn(3),MultiplyFn(GetFn(items[1].num,false),ConstantFn(2)))),
          |  ValStmt(y,MultiplyFn(AddFn(ConstantFn(3),GetFn(items[1].num,false)),ConstantFn(2)))
          |))""".stripMargin

      val lens = dynalens[Shipment]
      for {
        compiled <- Script.compile(script, lens)
      } yield assertTrue(normalize(compiled.toString) == normalize(expected))
    },

    test("complex list chain compiles to correct AST") {
      val script =
        """val xs = items.filter(_.qty > 5).distinct().sortDesc().limit(3)"""

      val expected =
        """BlockStmt(List(
          |  ValStmt(xs,
          |    LimitFn(
          |     SortDescFn(
          |       DistinctFn(
          |         FilterFn(
          |           GetFn(items,false),
          |           GreaterThanFn(GetFn(_.qty,false), ConstantFn(5))
          |         ),
          |         None
          |       ),
          |       None
          |     ),
          |     3
          |    )
          |  )
          |))""".stripMargin

      val lens = dynalens[Shipment]
      for {
        compiled <- Script.compile(script, lens)
      } yield assertTrue(normalize(compiled.toString) == normalize(expected))
    },

    test("map keys and values are typed correctly") {
      val script =
        """val ks = m.keys().distinct()
          |val vs = m.values().limit(2)
          |""".stripMargin

      val expected =
        """BlockStmt(List(
          |  ValStmt(ks,
          |    DistinctFn(
          |      KeysFn(GetFn(m,false)),
          |      None
          |    )
          |  ),
          |  ValStmt(vs,
          |    LimitFn(
          |      ValuesFn(GetFn(m,false)),
          |      2
          |    )
          |  )
          |))""".stripMargin
      val lens = dynalens[Mapped]
      for {
        compiled <- Script.compile(script, lens)
      } yield assertTrue(normalize(compiled.toString) == normalize(expected))
    },

    test("option-wrapped lists chain cleanly") {
      val script = """val ys = interest.filter(_.qty > 5).reverse().clean()"""
      val expected =
        """BlockStmt(List(
          |  ValStmt(ys,
          |    CleanFn(
          |      ReverseFn(
          |        FilterFn(
          |          GetFn(interest,true),
          |          GreaterThanFn(GetFn(_.qty,false), ConstantFn(5))
          |        )
          |      )
          |    )
          |  )
          |))""".stripMargin
      val lens = dynalens[Maybe]
      for {
        compiled <- Script.compile(script, lens)
      } yield assertTrue(normalize(compiled.toString) == normalize(expected))
    },

    test("map with option values supports keys.distinct.sortAsc") {
      val script = """val ps = props.keys().distinct().sortAsc()"""
      val expectedSubtrees = Seq("KeysFn", "DistinctFn", "SortAscFn")
      val lens = dynalens[Combo]
      for {
        compiled <- Script.compile(script, lens)
      } yield assertTrue(expectedSubtrees.forall(normalize(compiled.toString).contains))
    },

    // ------------------------------------------------------------------
    // Negative tests – must fail and give a meaningful error
    // ------------------------------------------------------------------

    test("reject sortAsc on a scalar Int") {
      val script = """val bad = qty.sortAsc()"""
      val lens   = dynalens[Item]
      for {
        r <- Script.compile(script, lens).either
      } yield assertTrue(r.left.exists(_.getMessage.contains("Error: Method 'sortAsc' cannot be applied to receiver of type scala.Int")))
    },

    test("reject keys on a List") {
      val script = """val bad = items.keys()"""
      val lens   = dynalens[Shipment]
      for {
        r <- Script.compile(script, lens).either
      } yield assertTrue(r.left.exists(_.getMessage.contains("Method 'keys' cannot be applied to receiver of type scala.collection.immutable.List[co.blocke.dynalens.Item]")))
    },

    test("reject filter on result of now() (String)") {
      val script = """val bad = now().filter(this == true)"""
      val lens   = dynalens[Shipment]
      for {
        r <- Script.compile(script, lens).either
      } yield assertTrue(r.left.exists(_.getMessage.contains("Method 'filter' cannot be applied to receiver of type java.util.Date")))
    },

    test("reject limit on a scalar Int") {
      val script = """val bad = qty.limit(2)"""
      val lens   = dynalens[Item]
      for {
        r <- Script.compile(script, lens).either
      } yield assertTrue(r.left.exists(_.getMessage.contains("Method 'limit' cannot be applied to receiver of type scala.Int")))
    },

    test("reject chaining keys.keys") {
      val script = """val bad = m.keys().keys()"""
      val lens   = dynalens[Mapped]
      for {
        r <- Script.compile(script, lens).either
      } yield assertTrue(r.left.exists(_.getMessage.contains("Method 'keys' cannot be applied to receiver of type scala.collection.immutable.List[java.lang.String]")))
    }
  )
}