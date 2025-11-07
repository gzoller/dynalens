package co.blocke.dynalens
package parser

import zio.*
import zio.test.*
import DynaLens.*
import CtxStrings.*
import co.blocke.testkit.ZioTestKit.*

object ParsingBooleanSpec extends ZIOSpecDefault {

  case class Item(name: String, num: Int, desc: Option[String], tags: List[String])

  def spec = suite("Parsing Boolean Tests")(

    test("If / else block") {
      val script =
        """
          |  val y = if num > 5 then "big" else "small"
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(y,IfFn(GreaterThanFn(GetFn(num,false,RootFn,[2,14],Some(ScalarType(num,scala.Int,false))),ConstantFn(5),[2,14]),ConstantFn(big),ConstantFn(small),[2,11]))))"""

      val expectedResult =
        """top -> Item(abc,6,Some(xyz),List(a, b))
          |y -> big
          |""".stripMargin

      val inst = Item("abc", 6, Some("xyz"), List("a", "b"))
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    },

    test("Boolean AND") {
      val script =
        """
          |  val ok = num > 1 && num < 10
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(ok,AndFn(GreaterThanFn(GetFn(num,false,RootFn,[2,12],Some(ScalarType(num,scala.Int,false))),ConstantFn(1),[2,12]),LessThanFn(GetFn(num,false,RootFn,[2,23],Some(ScalarType(num,scala.Int,false))),ConstantFn(10),[2,23]),[2,12]))))"""

      val expectedResult =
        """top -> Item(abc,6,Some(xyz),List(a, b))
          |ok -> true
          |""".stripMargin

      val inst = Item("abc", 6, Some("xyz"), List("a", "b"))
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(resultStr == expectedResult, compiled.toString == expectedCompiled)
    },

    test("Boolean OR") {
      val script =
        """
          |  val ok = num < 2 || num > 10
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(ok,OrFn(LessThanFn(GetFn(num,false,RootFn,[2,12],Some(ScalarType(num,scala.Int,false))),ConstantFn(2),[2,12]),GreaterThanFn(GetFn(num,false,RootFn,[2,23],Some(ScalarType(num,scala.Int,false))),ConstantFn(10),[2,23]),[2,12]))))"""

      val expectedResult =
        """top -> Item(abc,6,Some(xyz),List(a, b))
          |ok -> false
          |""".stripMargin

      val inst = Item("abc", 6, Some("xyz"), List("a", "b"))
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(resultStr == expectedResult, compiled.toString == expectedCompiled)
    },

    test("NOT operator") {
      val script =
        """
          |  val inv = ! (num > 5)
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(inv,NotFn(GreaterThanFn(GetFn(num,false,RootFn,[2,16],Some(ScalarType(num,scala.Int,false))),ConstantFn(5),[2,16]),[2,13]))))"""

      val expectedResult =
        """top -> Item(abc,6,Some(xyz),List(a, b))
          |inv -> false
          |""".stripMargin

      val inst = Item("abc", 6, Some("xyz"), List("a", "b"))
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(resultStr == expectedResult, compiled.toString == expectedCompiled)
    },

    test("Boolean operator precedence (NOT > AND > OR)") {
      val script =
        """
          |  val ok = !a || b && c
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(ok,OrFn(NotFn(ToBooleanFn(GetFn(a,false,RootFn,[2,13],Some(ScalarType(a,scala.Boolean,false))),[2,13]),[2,12]),AndFn(ToBooleanFn(GetFn(b,false,RootFn,[2,18],Some(ScalarType(b,scala.Boolean,false))),[2,18]),ToBooleanFn(GetFn(c,false,RootFn,[2,23],Some(ScalarType(c,scala.Boolean,false))),[2,23]),[2,18]),[2,12]))))"""

      val expectedResult =
        """top -> Item(false,false,true)
          |ok -> true
          |""".stripMargin

      case class Item(a: Boolean, b: Boolean, c: Boolean)
      val inst = Item(a = false, b = false, c = true)
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    },

    test("isDefined()") {
      val script =
        """
          |  val hasDesc = desc.isDefined()
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(hasDesc,IsDefinedFn(GetFn(desc,true,RootFn,[2,17],None),[1,1]))))"""

      val expectedResult =
        """top -> Item(abc,6,Some(xyz),List(a, b))
          |hasDesc -> true
          |""".stripMargin

      val inst = Item("abc", 6, Some("xyz"), List("a", "b"))
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        _ <- ZIO.succeed(println("!!! "+compiled))
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(resultStr == expectedResult, compiled.toString == expectedCompiled)
    },

    test("startsWith()") {
      val script =
        """
          |  val s = name.startsWith("a")
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(s,StartsWithFn(GetFn(name,false,RootFn,[2,11],None),ConstantFn(a),[1,1]))))"""

      val expectedResult =
        """top -> Item(abc,6,Some(xyz),List(a, b))
          |s -> true
          |""".stripMargin

      val inst = Item("abc", 6, Some("xyz"), List("a", "b"))
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(resultStr == expectedResult, compiled.toString == expectedCompiled)
    },

    test("endsWith()") {
      val script =
        """
          |  val s = name.endsWith("c")
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(s,EndsWithFn(GetFn(name,false,RootFn,[2,11],None),ConstantFn(c),[1,1]))))"""

      val expectedResult =
        """top -> Item(abc,6,Some(xyz),List(a, b))
          |s -> true
          |""".stripMargin

      val inst = Item("abc", 6, Some("xyz"), List("a", "b"))
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(resultStr == expectedResult, compiled.toString == expectedCompiled)
    },

    test("contains()") {
      val script =
        """
          |  val hasA = tags.contains("a")
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(hasA,ContainsFn(GetFn(tags,false,RootFn,[2,14],None),ConstantFn(a),[1,1]))))"""

      val expectedResult =
        """top -> Item(abc,6,Some(xyz),List(a, b))
          |hasA -> true
          |""".stripMargin

      val inst = Item("abc", 6, Some("xyz"), List("a", "b"))
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(resultStr == expectedResult, compiled.toString == expectedCompiled)
    },

    test("equalsIgnoreCase()") {
      val script =
        """
          |  val eq = name.equalsIgnoreCase("ABC")
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(eq,EqualsIgnoreCaseFn(GetFn(name,false,RootFn,[2,12],None),ConstantFn(ABC),[1,1]))))"""

      val expectedResult =
        """top -> Item(abc,6,Some(xyz),List(a, b))
          |eq -> true
          |""".stripMargin

      val inst = Item("abc", 6, Some("xyz"), List("a", "b"))
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(resultStr == expectedResult, compiled.toString == expectedCompiled)
    },

    test("matchesRegex()") {
      val script =
        """
          |  val ok = name.matchesRegex("^a.*c$")
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(ok,MatchesRegexFn(GetFn(name,false,RootFn,[2,12],None),ConstantFn(^a.*c$),[1,1]))))"""

      val expectedResult =
        """top -> Item(abc,6,Some(xyz),List(a, b))
          |ok -> true
          |""".stripMargin

      val inst = Item("abc", 6, Some("xyz"), List("a", "b"))
      val lens = into[Item]

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(resultStr == expectedResult, compiled.toString == expectedCompiled)
    }
  ) @@ ziotestkit
}