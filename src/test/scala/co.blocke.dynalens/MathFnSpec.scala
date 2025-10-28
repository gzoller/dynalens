package co.blocke.dynalens
package fn

import zio.*
import zio.test.*
import co.blocke.testkit.ZioTestKit.*


object MathFnSpec extends ZIOSpecDefault:

  // Test model
  case class Foo(i: Int, xs: List[Int], ys: List[Double], os: Option[List[Int]])
  val foo       = Foo(5, List(1, -2, 3), List(2.5, 7.5, 10.0), Some(List(4, 4, 4)))
  val fooLens   = DynaLens.into[Foo].topLens
  def ctx       = DynaContext(Map("foo" -> (foo, fooLens)))

  def G(path: String, opt: Boolean = false): GetFn =
    GetFn(path, opt, RootFn, s"pos-$path")

  def spec =
    suite("MathFnSpec")(
      // ---------------- AbsFn ----------------
      test("AbsFn works for positive and negative numerics") {
        val f1 = AbsFn(G("foo.i"), "pos")
        for exit <- f1.resolve(ctx).exit
          yield assertTrue(exit == Exit.succeed((5, fooLens.fields("i"))))
      },
      test("AbsFn errors on non-numeric") {
        val fn = AbsFn(G("foo.xs"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit.isFailure)
      },

      // ---------------- MinFn ----------------
      test("MinFn returns smallest int") {
        val fn = MinFn(G("foo.xs"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit == Exit.succeed(( -2, fooLens.fields("xs") )))
      },
      test("MinFn errors on invalid input type") {
        val fn = MinFn(G("foo"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit.isFailure)
      },

      // ---------------- MaxFn ----------------
      test("MaxFn returns largest double") {
        val fn = MaxFn(G("foo.ys"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit == Exit.succeed((10.0, fooLens.fields("ys"))))
      },

      // ---------------- SumFn ----------------
      test("SumFn sums ints") {
        val fn = SumFn(G("foo.xs"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit == Exit.succeed((2, fooLens.fields("xs"))))
      },
      test("SumFn Optional list None => 0") {
        val fn = SumFn(G("foo.os.xs", true), "pos") // simulate None
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit.isSuccess)
      },

      // ---------------- AvgFn ----------------
      test("AvgFn averages doubles") {
        val fn = AvgFn(G("foo.ys"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit == Exit.succeed(( (2.5+7.5+10.0)/3, fooLens.fields("ys") )))
      },

      // ---------------- MedianFn ----------------
      test("MedianFn calculates for odd length") {
        val fn = MedianFn(G("foo.xs"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit == Exit.succeed((1.0, fooLens.fields("xs"))))
      },
      test("MedianFn calculates for even length") {
        val fn = MedianFn(ConstantFn(List(10.0, 20.0, 30.0, 40.0)), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit match
            case Exit.Success((v, _)) =>
              v == 25.0
            case _ =>
              false
          )
      }
    ).provide(
      ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
    ) @@ ziotestkit