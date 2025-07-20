package co.blocke.dynalens

import zio.*
import zio.test.*

object SimpleFnsSpec extends ZIOSpecDefault:

  val testCtx: Map[String, (Any, DynaLens[?])] = Map(
    "top" -> ("hello", null),
    "s1" -> ("world", null),
    "num" -> (5, null),
    "x" -> (10, null),
    "y" -> (3, null)
  )

  def spec = suite("SimpleFnsSpec")(
    test("ConstantFn should return constant value") {
      for {
        result <- ConstantFn("foo").resolve(testCtx)
      } yield assertTrue(result == "foo")
    },
    test("GetFn should return context value") {
      for {
        result <- GetFn("s1").resolve(testCtx)
      } yield assertTrue(result == "world")
    },
    test("AddFn should sum two numbers") {
      val fn = AddFn(GetFn("x"), GetFn("y"))
      for {
        result <- fn.resolve(testCtx)
      } yield assertTrue(result == 13)
    },
    test("SubFn should subtract two numbers") {
      val fn = SubtractFn(GetFn("x"), GetFn("y"))
      for {
        result <- fn.resolve(testCtx)
      } yield assertTrue(result == 7)
    },
    test("ConcatFn should concatenate strings") {
      val fn = ConcatFn(List(GetFn("top"), GetFn("s1")))
      for {
        result <- fn.resolve(testCtx)
      } yield assertTrue(result == "helloworld")
    },
    test("ToUpperFn should convert string to upper case") {
      val fn = ToUpperFn(GetFn("s1"))
      for {
        result <- fn.resolve(testCtx)
      } yield assertTrue(result == "WORLD")
    },
    test("SubstringFn should extract substring") {
      val fn = SubstringFn(GetFn("top"), ConstantFn(1), Some(ConstantFn(4)))
      for {
        result <- fn.resolve(testCtx)
      } yield assertTrue(result == "ell")
    },
    test("Bad cast in Fn should fail") {
      val badCtx = testCtx.updated("x", ("oops", null))
      val fn = AddFn(GetFn("x"), ConstantFn(5))
      for {
        result <- fn.resolve(badCtx).either
      } yield assertTrue(result.isLeft)
    }
  ).provide(BiMapRegistry.layer(EmptyBiMapRegistry))
