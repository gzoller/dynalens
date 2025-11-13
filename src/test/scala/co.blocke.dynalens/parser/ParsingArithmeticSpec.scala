package co.blocke.dynalens
package parser

import zio.*
import zio.test.*
import DynaLens.*
import co.blocke.testkit.ZioTestKit.*

object ParsingArithmeticSpec extends ZIOSpecDefault {

  case class Item(qty: Int, price: Int, tax: Int)

  def spec = suite("Parsing Arithmetic Tests")(

    test("Simple addition") {
      val script =
        """
          |  val z = 2 + 3
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(z,AddFn(ConstantFn(2),ConstantFn(3),ScalarType(,scala.Int,false),[2,11]))))"""

      val expectedResult =
        """top -> Item(5,10,2)
          |z -> 5
          |""".stripMargin

      val inst = Item(5, 10, 2)
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = ctx.toString
      } yield assertTrue(
        updated == inst,
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    },

    test("Chained addition and subtraction") {
      val script =
        """
          |  val x = 10 + 5 - 3
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,SubtractFn(AddFn(ConstantFn(10),ConstantFn(5),ScalarType(,scala.Int,false),[2,11]),ConstantFn(3),ScalarType(,scala.Int,false),[2,11]))))"""

      val expectedResult =
        """top -> Item(1,2,3)
          |x -> 12
          |""".stripMargin

      val inst = Item(1, 2, 3)
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = ctx.toString
      } yield assertTrue(
        updated == inst,
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    },

    test("Multiplication and division") {
      val script =
        """
          |  val a = 8 * 2 / 4
          |  qty = a
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(a,DivideFn(MultiplyFn(ConstantFn(8),ConstantFn(2),ScalarType(,scala.Int,false),[2,11]),ConstantFn(4),ScalarType(,scala.Double,false),[2,11])), UpdateStmt(qty,GetFn(a,false,RootFn,[3,9],Some(ScalarType(a,scala.Double,false))),[3,9],ScalarType(qty,scala.Int,false))))"""

      val expectedResult =
        """top -> Item(4,5,1)
          |a -> 4.0
          |""".stripMargin

      val inst = Item(19, 5, 1)
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = ctx.toString
      } yield assertTrue(
        updated == Item(4,5,1),
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    },

    test("Parentheses precedence") {
      val script =
        """
          |  val n = (2 + 3) * 4
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(n,MultiplyFn(AddFn(ConstantFn(2),ConstantFn(3),ScalarType(,scala.Int,false),[2,12]),ConstantFn(4),ScalarType(,scala.Int,false),[2,11]))))"""

      val expectedResult =
        """top -> Item(0,0,0)
          |n -> 20
          |""".stripMargin

      val inst = Item(0, 0, 0)
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = ctx.toString
      } yield assertTrue(
        updated == inst,
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    },

    test("Mixed arithmetic on context variables") {
      val script =
        """
          |  val total = qty * price + tax
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(total,AddFn(MultiplyFn(GetFn(qty,false,RootFn,[2,15],Some(ScalarType(qty,scala.Int,false))),GetFn(price,false,RootFn,[2,21],Some(ScalarType(price,scala.Int,false))),ScalarType(,scala.Int,false),[2,15]),GetFn(tax,false,RootFn,[2,29],Some(ScalarType(tax,scala.Int,false))),ScalarType(,scala.Int,false),[2,15]))))"""

      val expectedResult =
        """top -> Item(2,5,3)
          |total -> 13
          |""".stripMargin

      val inst = Item(2, 5, 3)
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = ctx.toString
      } yield assertTrue(
        updated == inst,
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    },

    test("Invalid arithmetic (type mismatch)") {
      val script =
        """
          |  val bad = "abc" * 3
          |""".stripMargin

      val lens = into[Item]

      for {
        compiledAttempt <- Script.compile(script, lens).either
      } yield assertTrue(
        compiledAttempt.isLeft
      )
    }
  ) @@ ziotestkit
}