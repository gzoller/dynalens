package co.blocke.dynalens
package parser

import zio.*
import zio.test.*
import DynaLens.*
import CtxStrings.*
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
        """BlockStmt(List(ValStmt(z,AddFn(ConstantFn(2),List(ConstantFn(3)),[1,1]))))"""

      val expectedResult =
        """top -> Item(5,10,2)
          |z -> 5
          |""".stripMargin

      val inst = Item(5, 10, 2)
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
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
        """BlockStmt(List(ValStmt(x,SubtractFn(AddFn(ConstantFn(10),List(ConstantFn(5)),[1,1]),List(ConstantFn(3)),[1,1]))))"""

      val expectedResult =
        """top -> Item(1,2,3)
          |x -> 12
          |""".stripMargin

      val inst = Item(1, 2, 3)
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
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
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(a,DivideFn(MultiplyFn(ConstantFn(8),List(ConstantFn(2)),[1,1]),List(ConstantFn(4)),[1,1]))))"""

      val expectedResult =
        """top -> Item(19,5,1)
          |a -> 4.0
          |""".stripMargin

      val inst = Item(19, 5, 1)
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst,
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
        """BlockStmt(List(ValStmt(n,MultiplyFn(AddFn(ConstantFn(2),List(ConstantFn(3)),[1,1]),List(ConstantFn(4)),[1,1]))))"""

      val expectedResult =
        """top -> Item(0,0,0)
          |n -> 20
          |""".stripMargin

      val inst = Item(0, 0, 0)
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
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
        """BlockStmt(List(ValStmt(total,AddFn(MultiplyFn(GetFn(qty,false,RootFn,[1,1]),List(GetFn(price,false,RootFn,[1,1])),[1,1]),List(GetFn(tax,false,RootFn,[1,1])),[1,1]))))"""

      val expectedResult =
        """top -> Item(2,5,3)
          |total -> 13
          |""".stripMargin

      val inst = Item(2, 5, 3)
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
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

      val inst = Item(1, 2, 3)
      val lens = into[Item]

      for {
        compiledAttempt <- Script.compile(script, lens).either
      } yield assertTrue(
        compiledAttempt.isLeft
      )
    }
  ) @@ ziotestkit
}