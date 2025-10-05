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
        """BlockStmt(List(ValStmt(x,AddFn(ConstantFn(3),MultiplyFn(GetFn(num,false,Some(IndexFn(GetFn(items,false,None),1))),ConstantFn(2)))), ValStmt(y,MultiplyFn(AddFn(ConstantFn(3),GetFn(num,false,Some(IndexFn(GetFn(items,false,None),1)))),ConstantFn(2)))))""".stripMargin

      val lens = dynalens[Shipment]
      for {
        compiled <- Script.compile(script, lens)
      } yield assertTrue(normalize(compiled.toString) == normalize(expected))
    },

    test("complex list chain compiles to correct AST") {
      val script =
        """val xs = items.filter(this.qty > 5).distinct().sortDesc().limit(3)"""

      val expected =
        """BlockStmt(
          |  List(
          |    ValStmt(
          |      xs,
          |      LimitFn(
          |        SortDescFn(
          |          DistinctFn(
          |            FilterFn(
          |              GetFn(items,false,None),
          |              GreaterThanFn(
          |                GetFn(qty,false,Some(GetFn(items,false,None))),
          |                ConstantFn(5)
          |              )
          |            ),
          |            None
          |          ),
          |          None
          |        ),
          |        3
          |      )
          |    )
          |  )
          |)""".stripMargin

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
          |      KeysFn(GetFn(m,false,None)),
          |      None
          |    )
          |  ),
          |  ValStmt(vs,
          |    LimitFn(
          |      ValuesFn(GetFn(m,false,None)),
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
          |          GetFn(interest,true,None),
          |          GreaterThanFn(GetFn(qty,false,Some(GetFn(interest,true,None))), ConstantFn(5))
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
      val expected = """BlockStmt(List(ValStmt(ps,SortAscFn(DistinctFn(KeysFn(GetFn(props,false,None)),None),None))))"""
      val lens = dynalens[Combo]
      for {
        compiled <- Script.compile(script, lens)
      } yield assertTrue(normalize(compiled.toString) == normalize(expected))
    },

    test("propagate Option through filter+sum on Option[List[Int]]") {
      val script = """val s = l2.filter(this > 3).sum()"""
      val expected = """BlockStmt(List(ValStmt(s,SumFn(FilterFn(GetFn(l2,true,None),GreaterThanFn(GetFn(this,false,Some(GetFn(l2,true,None))),ConstantFn(3)))))))"""
      val lens   = dynalens[OptTest]
      for {
        compiled <- Script.compile(script, lens)
      } yield assertTrue(normalize(compiled.toString) == normalize(expected))
    },

    test("propagate Option through distinct+sortAsc+max on Option[List[Int]]") {
      val script = """val s = l2.distinct().sortAsc().max()"""
      val expected = """BlockStmt(List(ValStmt(s,MaxFn(SortAscFn(DistinctFn(GetFn(l2,true,None),None),None)))))"""
      val lens   = dynalens[OptTest]
      for {
        compiled <- Script.compile(script, lens)
      } yield assertTrue(normalize(compiled.toString) == normalize(expected))
    },

    test("propagate Option through filter on Option[List[Int]] with this inside predicate") {
      val script =
        """val x = l2.filter(this % 2 == 0)"""
      val expected =
        """BlockStmt(List(ValStmt(x,FilterFn(GetFn(l2,true,None),EqualFn(ModuloFn(GetFn(this,false,Some(GetFn(l2,true,None))),ConstantFn(2)),ConstantFn(0))))))"""
      val lens = dynalens[OptTest]
      for {
        compiled <- Script.compile(script, lens)
      } yield assertTrue(normalize(compiled.toString) == normalize(expected))
    },

    test("map-like statement with this inside predicate on Option[List[Int]]") {
      val script =
        """l2.filter(this < 2)"""
      val expected =
           """BlockStmt(List(MapStmt(l2,FilterFn(GetFn(l2,true,None),LessThanFn(GetFn(this,false,Some(GetFn(l2,true,None))),ConstantFn(2))))))"""
      val lens = dynalens[OptTest]
      for {
        compiled <- Script.compile(script, lens)
      } yield assertTrue(
        normalize(compiled.toString) == normalize(expected)
      )
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
      } yield assertTrue(r.left.exists(_.getMessage.contains("Error: Method 'keys' cannot be applied to receiver of type scala.collection.immutable.List[co.blocke.dynalens.Item]")))
    },

    test("reject filter on result of now() (String)") {
      val script = """val bad = now().filter(this == true)"""
      val lens   = dynalens[Shipment]
      for {
        r <- Script.compile(script, lens).either
      } yield assertTrue(r.left.exists(_.getMessage.contains("Error: Method 'filter' cannot be applied to receiver of type java.time.LocalDateTime")))
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
    },

    test("reject comparing Option[Int] directly inside filter") {
      val script = """val s = nums.filter(this > maybeInt)"""
      val lens   = dynalens[OptTest]
      for {
        r <- Script.compile(script, lens).either
      } yield assertTrue(
        r.left.exists(_.getMessage.contains("Error: > operands cannot be optional"))
      )
    },

    test("string val inferred and used inside numeric filter") {
      val script =
        """val x = "foo" + "bar"
          |val s = nums.filter(this > x)
          |""".stripMargin
      val lens = dynalens[OptTest]   // adjust type param to match your schema that defines `nums: List[Int]`
      for {
        r <- Script.compile(script, lens).either
      } yield assertTrue(
        r.left.exists(_.getMessage.contains(" Error: > operands must be numeric (found scala.Int, java.lang.String)"))
      )
    },

    test("reject using non-numeric value with arithmetic function") {
      val script = """val s = nums.filter(this > true)"""
      val lens   = dynalens[OptTest]
      for {
        r <- Script.compile(script, lens).either
      } yield assertTrue(
        r.left.exists(_.getMessage.contains("Error: > operands must be numeric (found scala.Int, scala.Boolean)"))
      )
    },

    test("reject arithmetic on Option[Int] without else()") {
      val script = """val s = maybeInt + 5"""
      val lens   = dynalens[OptTest]
      for {
        r <- Script.compile(script, lens).either
      } yield assertTrue(
        r.left.exists(_.getMessage.contains("Error: + requires numeric operands, found scala.Option and scala.Int"))
      )
    },

    test("rejected statement should resolve predicate types") {
      val script =
        """l2.filter(this < true)"""
      val lens   = dynalens[OptTest]
      for {
        r <- Script.compile(script, lens).either
      } yield assertTrue(
        r.left.exists(_.getMessage.contains("Error: < operands must be numeric (found scala.Int, scala.Boolean)"))
      )
    }
  )
}

