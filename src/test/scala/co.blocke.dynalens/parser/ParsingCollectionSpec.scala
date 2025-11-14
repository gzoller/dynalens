package co.blocke.dynalens
package parser

import zio.*
import zio.test.*
import DynaLens.*
import co.blocke.testkit.ZioTestKit.*

object ParsingCollectionSpec extends ZIOSpecDefault {

  case class Wrap(list: List[Any], mmap: Map[String,Int], nums: List[Int], nested: List[Option[Int]])
  val lens = into[Wrap]

  def spec = suite("Parsing CollectionFn Tests")(

    test("keys() extracts all map keys") {
      val script =
        """
          |  val ks = mmap.keys()
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(ks,KeysFn(GetFn(mmap,false,RootFn,[2,12],Some(MapType(mmap,ScalarType(mmap,java.lang.String,false),ScalarType(mmap,scala.Int,false),scala.collection.immutable.Map[java.lang.String,scala.Int],false))),[2,21]))))"""

      val inst = Wrap(Nil, Map("a" -> 1, "b" -> 2), Nil, Nil)

      val expectedResult =
        """top -> Wrap(List(),Map(a -> 1, b -> 2),List(),List())
          |ks -> List(a, b)
          |""".stripMargin

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = ctx.toString
        _ <- ZIO.succeed(println("XX---> "+compiled))
        _ <- ZIO.succeed(println("----1> "+resultStr))
      } yield assertTrue(
        updated == inst,
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    },

    test("values() extracts all map values") {
      val script =
        """
          |  val vs = mmap.values()
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(ValStmt(vs,ValuesFn(GetFn(mmap,false,RootFn,[2,12],Some(MapType(mmap,ScalarType(mmap,java.lang.String,false),ScalarType(mmap,scala.Int,false),scala.collection.immutable.Map[java.lang.String,scala.Int],false))),[2,23]))))"""

      val inst = Wrap(Nil, Map("a" -> 1, "b" -> 2), Nil, Nil)
      val expectedResult =
        """top -> Wrap(List(),Map(a -> 1, b -> 2),List(),List())
          |vs -> List(1, 2)
          |""".stripMargin

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

    test("filter() retains matching list elements") {
      val script =
        """
          |  nums = nums.filter(this > 2)
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(UpdateStmt(nums,FilterFn(GetFn(nums,false,RootFn,[2,10],Some(ListType(nums,ScalarType(nums,scala.Int,false),scala.collection.immutable.List[scala.Int],false))),GreaterThanFn(IterThisFn,ConstantFn(2),[2,22]),[2,21]),[2,10],ListType(nums,ScalarType(nums,scala.Int,false),scala.collection.immutable.List[scala.Int],false))))"""

      val inst = Wrap(Nil, Map(), List(1, 3, 2, 5), Nil)
      val expectedResult =
        """top -> Wrap(List(),Map(),List(3, 5),List())
          |""".stripMargin

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = ctx.toString
      } yield assertTrue(
        updated == Wrap(Nil, Map(), List(3,5), Nil),
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    },

    test("nested call with contains uses outer predicate 'this' correctly") {
      val script =
        """
          |  nums = nums.filter(nums.contains(5 + this))
          |""".stripMargin

      val expectedCompiled =
        """BlockStmt(List(UpdateStmt(nums,FilterFn(GetFn(nums,false,RootFn,[2,10],Some(ListType(nums,ScalarType(nums,scala.Int,false),scala.collection.immutable.List[scala.Int],false))),ContainsFn(GetFn(nums,false,RootFn,[2,22],Some(ListType(nums,ScalarType(nums,scala.Int,false),scala.collection.immutable.List[scala.Int],false))),AddFn(ConstantFn(5),IterThisFn,ScalarType(,scala.Int,false),[2,36]),[2,35]),[2,21]),[2,10],ListType(nums,ScalarType(nums,scala.Int,false),scala.collection.immutable.List[scala.Int],false))))"""

      val inst = Wrap(Nil, Map(), List(1, 7, 5, 2, 6, 9), Nil)
      val expectedResult =
        """top -> Wrap(List(),Map(),List(1, 2),List())
          |""".stripMargin // actual order not critical; focus on substitution correctness

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = ctx.toString
      } yield assertTrue(
        updated == Wrap(Nil, Map(), List(1,2), Nil),
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
      val expectedResult =
        """top -> Wrap(List(),Map(),List(3, 2, 1),List())
          |""".stripMargin

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = ctx.toString
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
      val expectedResult =
        """top -> Wrap(List(),Map(),List(),List(Some(5), Some(9)))
          |""".stripMargin

      for {
        compiled <- Script.compile(script, lens)
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = ctx.toString
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
        (updated, ctx) <- lens.run(compiled, inst)
        resultStr = ctx.toString
      } yield assertTrue(
        updated == WrapOpt(Nil, Map(), Nil, Some(List(Some(5), Some(9)))),
        resultStr == expectedResult,
        compiled.toString == expectedCompiled
      )
    }
  ) @@ ziotestkit
}

// TODO: Test these:
/*
myField = myField.map(this)
myField = myField.filter(myField.otherField.contains(this))s
nums = nums.filter({
  val foo = 2
  this * foo
})
*/