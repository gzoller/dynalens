package co.blocke.dynalens
package parser

import zio.*
import zio.test.*
import DynaLens.*
import CtxStrings.*
import co.blocke.testkit.ZioTestKit.*


object ParsingSmokeSpec extends ZIOSpecDefault {

  def spec = suite("Parsing Smoke Tests")(

    test("Simple val assignment script") {
      val script =
        """
          |  val x = 42
          |  val y = x + 8
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(x,ConstantFn(42)), ValStmt(y,AddFn(GetFn(x,false,RootFn,[3,11],Some(ScalarType(x,scala.Int,false))),ConstantFn(8),ScalarType(,scala.Int,false),[3,11]))))"""

      val expectedResult =
        """top -> Item(abc,2,5)
          |x -> 42
          |y -> 50""".stripMargin + "\n"

      val inst = Item("abc", 2, 5)
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

    test("Simple update statement") {
      val script =
        """
          |  num = num * 2
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(UpdateStmt(num,MultiplyFn(GetFn(num,false,RootFn,[2,9],Some(ScalarType(num,scala.Int,false))),ConstantFn(2),ScalarType(,scala.Int,false),[2,9]),[2,9],ScalarType(num,scala.Int,false))))"""

      val expectedResult =
        """top -> Item(abc,2,10)
          |""".stripMargin

      val inst = Item("abc", 2, 5)
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == Item("abc", 2, 10),
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    }
  ) @@ ziotestkit
}