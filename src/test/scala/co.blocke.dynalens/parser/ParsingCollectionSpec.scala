package co.blocke.dynalens
package parser

import zio.*
import zio.test.*
import DynaLens.*
import CtxStrings.*
import co.blocke.testkit.ZioTestKit.*

object ParsingCollectionSpec extends ZIOSpecDefault {

  case class Wrap(list: List[Any], map: Map[String,Int], nums: List[Int], nested: List[Option[Int]])

  def spec = suite("Parsing CollectionFn Tests")(

    test("keys() extracts all map keys") {
      val script =
        """
          |  val ks = map.keys()
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(ks,KeysFn(GetFn(map,false,RootFn,[2,13],Some(MapType(map,ScalarType(,java.lang.String,false),ScalarType(,scala.Int,false),java.util.Map))),[2,13]))))"""

      val inst = Wrap(Nil, Map("a" -> 1, "b" -> 2), Nil, Nil)
      val lens = into[Wrap]

      val expectedResult =
        """top -> Wrap(List(),Map(a -> 1, b -> 2),List(),List())
          |ks -> List(a, b)
          |""".stripMargin

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

    test("values() extracts all map values") {
      val script =
        """
          |  val vs = map.values()
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(vs,ValuesFn(GetFn(map,false,RootFn,[2,13],Some(MapType(map,ScalarType(,java.lang.String,false),ScalarType(,scala.Int,false),java.util.Map))),[2,13]))))"""

      val inst = Wrap(Nil, Map("a" -> 1, "b" -> 2), Nil, Nil)
      val lens = into[Wrap]
      val expectedResult =
        """top -> Wrap(List(),Map(a -> 1, b -> 2),List(),List())
          |vs -> List(1, 2)
          |""".stripMargin

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == inst,
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    } @@only,

    test("filter() retains matching list elements") {
      val script =
        """
          |  nums = nums.filter(_ > 2)
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(UpdateStmt(nums,FilterFn(GetFn(nums,false,RootFn,[2,11],Some(ListType(nums,ScalarType(,scala.Int,false),scala.List[Int]))),LambdaFn(_,GreaterThanFn(GetFn(_,false,RootFn,[2,23],Some(ScalarType(,scala.Int,false))),ConstantFn(2),ScalarType(,scala.Boolean,false),[2,23]),[2,23]),[2,11]),[2,11],ListType(nums,ScalarType(,scala.Int,false),scala.List[Int]))))"""

      val inst = Wrap(Nil, Map(), List(1, 3, 2, 5), Nil)
      val lens = into[Wrap]
      val expectedResult =
        """top -> Wrap(List(),Map(),List(3, 5),List())
          |""".stripMargin

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == Wrap(Nil, Map(), List(3,5), Nil),
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    },

    test("reverse() reverses list elements") {
      val script =
        """
          |  nums = nums.reverse()
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(UpdateStmt(nums,ReverseFn(GetFn(nums,false,RootFn,[2,11],Some(ListType(nums,ScalarType(,scala.Int,false),scala.List[Int]))),[2,11]),[2,11],ListType(nums,ScalarType(,scala.Int,false),scala.List[Int]))))"""

      val inst = Wrap(Nil, Map(), List(1,2,3), Nil)
      val lens = into[Wrap]
      val expectedResult =
        """top -> Wrap(List(),Map(),List(3, 2, 1),List())
          |""".stripMargin

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == Wrap(Nil, Map(), List(3,2,1), Nil),
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    },

    test("clean() removes nulls and Nones") {
      val script =
        """
          |  nested = nested.clean()
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(UpdateStmt(nested,CleanFn(GetFn(nested,false,RootFn,[2,12],Some(ListType(nested,ScalarType(nested,scala.Int,true),scala.collection.immutable.List[scala.Option[scala.Int]],false))),ListType(nested,ScalarType(nested,scala.Int,true),scala.collection.immutable.List[scala.Option[scala.Int]],false),[1,1]),[2,12],ListType(nested,ScalarType(nested,scala.Int,true),scala.collection.immutable.List[scala.Option[scala.Int]],false))))"""

      val inst = Wrap(Nil, Map(), Nil, List(Some(5), None, null, Some(9)))
      val lens = into[Wrap]
      val expectedResult =
        """top -> Wrap(List(),Map(),List(),List(Some(5), Some(9)))
          |""".stripMargin

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == Wrap(Nil, Map(), Nil, List(Some(5), Some(9))),
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    },

    test("clean() removes nulls and Nones in Option[List[_]]") {
      val script =
        """
          |  optnums = optnums.clean()
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(UpdateStmt(optnums,CleanFn(GetFn(optnums,true,RootFn,[2,13],Some(ListType(optnums,ScalarType(optnums,scala.Int,true),scala.collection.immutable.List[scala.Option[scala.Int]],true))),ListType(optnums,ScalarType(optnums,scala.Int,true),scala.collection.immutable.List[scala.Option[scala.Int]],false),[1,1]),[2,13],ListType(optnums,ScalarType(optnums,scala.Int,true),scala.collection.immutable.List[scala.Option[scala.Int]],true))))"""

      case class WrapOpt(list: List[Any], map: Map[String,Int], nums: List[Int], optnums: Option[List[Option[Int]]])
      val inst = WrapOpt(Nil, Map(), Nil, Some(List(Some(5), None, null, Some(9))))
      val lens = into[WrapOpt]
      val expectedResult =
        """top -> WrapOpt(List(),Map(),List(),Some(List(Some(5), Some(9))))
          |""".stripMargin

      for {
        compiled <- Script.compile(script, lens)
        _ <- ZIO.succeed(println(">>> "+compiled))
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = toStringCtx(ctx)
      } yield assertTrue(
        updated == WrapOpt(Nil, Map(), Nil, Some(List(Some(5), Some(9)))),
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    }
  ) @@ ziotestkit
}