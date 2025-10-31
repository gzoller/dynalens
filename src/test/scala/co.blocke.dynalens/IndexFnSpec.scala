package co.blocke.dynalens
package fn

import zio.*
import zio.test.*
import co.blocke.testkit.ZioTestKit.*


object IndexFnSpec extends ZIOSpecDefault:

  // Helper: make a simple context with a root symbol 'root'
  def buildCtx(symbol: String, value: Any, isOpt: Boolean = false): DynaContext =
    val lens = ScalarLens(symbol, isOptional = isOpt, parent = None)
    DynaContext(Map.empty, TestHelpers.emptyDL).bind(symbol, value, lens)

  def spec = suite("IndexFnSpec") (

    // --------------------------------------------------------------------------
    // Core: List indexing
    // --------------------------------------------------------------------------
    test("List: valid index returns element wrapped in Option") {
      val ctx = buildCtx("root", List("a", "b", "c"))
      val fn = IndexFn(GetFn("root", false, RootFn, "pos"), ConstantFn(1), "pos")

      for result <- fn.resolve(ctx)
        yield assertTrue(result._1 == Some("b"))
    },

    test("List: out-of-range index returns None") {
      val ctx = buildCtx("root", List(10, 20))
      val fn = IndexFn(GetFn("root", false, RootFn, "pos"), ConstantFn(5), "pos")

      for result <- fn.resolve(ctx)
        yield assertTrue(result._1 == None)
    },

    test("List: invalid index type fails") {
      val ctx = buildCtx("root", List(1, 2, 3))
      val fn = IndexFn(GetFn("root", false, RootFn, "pos"), ConstantFn("wow"), "pos")

      for result <- fn.resolve(ctx).either
        yield assertTrue(result.isLeft)
    },

    // --------------------------------------------------------------------------
    // Core: Map indexing
    // --------------------------------------------------------------------------
    test("Map: existing key returns value wrapped in Option") {
      val ctx = buildCtx("root", Map("x" -> 100, "y" -> 200))
      val fn = IndexFn(GetFn("root", false, RootFn, "pos"), ConstantFn("y"), "pos")

      for result <- fn.resolve(ctx)
        yield assertTrue(result._1 == Some(200))
    },

    test("Map: missing key returns None") {
      val ctx = buildCtx("root", Map("x" -> 100))
      val fn = IndexFn(GetFn("root", false, RootFn, "pos"), ConstantFn("zzz"), "pos")

      for result <- fn.resolve(ctx)
        yield assertTrue(result._1 == None)
    },

    test("Map: numeric key works when present") {
      val ctx = buildCtx("root", Map(1 -> "A", 2 -> "B"))
      val fn = IndexFn(GetFn("root", false, RootFn, "pos"), ConstantFn(2), "pos")

      for result <- fn.resolve(ctx)
        yield assertTrue(result._1 == Some("B"))
    },

    // --------------------------------------------------------------------------
    // Option receivers
    // --------------------------------------------------------------------------
    test("Option[List]: Some(list) behaves like List") {
      val ctx = buildCtx("root", Some(List("a", "b", "c")), true)
      val fn = IndexFn(GetFn("root", true, RootFn, "pos"), ConstantFn(2), "pos")

      for result <- fn.resolve(ctx)
        yield assertTrue(result._1 == Some("c"))
    },

    test("Option[List]: None receiver returns None") {
      val ctx = buildCtx("root", None, true)
      val fn = IndexFn(GetFn("root", true, RootFn, "pos"), ConstantFn(1), "pos")

      for result <- fn.resolve(ctx)
        yield assertTrue(result._1 == None)
    },

    test("Option[Map]: Some(map) behaves like Map") {
      val ctx = buildCtx("root", Some(Map("a" -> 1, "b" -> 2)), true)
      val fn = IndexFn(GetFn("root", true, RootFn, "pos"), ConstantFn("b"), "pos")

      for result <- fn.resolve(ctx)
        yield assertTrue(result._1 == Some(2))
    },

    test("Option[Map]: None receiver returns None") {
      val ctx = buildCtx("root", None, true)
      val fn = IndexFn(GetFn("root", true, RootFn, "pos"), ConstantFn("x"), "pos")

      for result <- fn.resolve(ctx)
        yield assertTrue(result._1 == None)
    },

    // --------------------------------------------------------------------------
    // Curried / chained indexing
    // --------------------------------------------------------------------------
    test("Curried indexing: List[List[Int]]") {
      val ctx = buildCtx("root", List(List(10, 11), List(20, 21)))
      val fn = IndexFn(
        IndexFn(GetFn("root", false, RootFn, "pos"), ConstantFn(1), "pos"),
        ConstantFn(0),
        "pos"
      )

      for result <- fn.resolve(ctx)
        yield assertTrue(result._1 == Some(20))
    },

    test("Curried indexing: Map[String, List[String]]") {
      val ctx = buildCtx("root", Map("a" -> List("x", "y"), "b" -> List("p", "q")))
      val inner = IndexFn(GetFn("root", false, RootFn, "pos"), ConstantFn("b"), "pos")
      val outer = IndexFn(inner, ConstantFn(1), "pos")

      for result <- outer.resolve(ctx)
        yield assertTrue(result._1 == Some("q"))
    },

    test("Curried indexing: Option[List[List[String]]]") {
      val ctx = buildCtx("root", Some(List(List("A", "B"), List("C", "D"))), true)
      val fn = IndexFn(IndexFn(GetFn("root", true, RootFn, "pos"), ConstantFn(1), "pos"), ConstantFn(0), "pos")

      for result <- fn.resolve(ctx)
        yield assertTrue(result._1 == Some("C"))
    },

    // --------------------------------------------------------------------------
    // Mixed receiver types
    // --------------------------------------------------------------------------
    test("Receiver is non-indexable type (Int) fails") {
      val ctx = buildCtx("root", 42)
      val fn = IndexFn(GetFn("root", false, RootFn, "pos"), ConstantFn(0), "pos")

      for result <- fn.resolve(ctx).either
        yield assertTrue(result.isLeft)
    },

    test("Receiver is null fails gracefully") {
      val ctx = buildCtx("root", null)
      val fn = IndexFn(GetFn("root", false, RootFn, "pos"), ConstantFn(0), "pos")

      for result <- fn.resolve(ctx).either
        yield assertTrue(result.isLeft)
    },

    // --------------------------------------------------------------------------
    // Nested GetFn (compile-time path concat)
    // --------------------------------------------------------------------------
    test("IndexFn(GetFn) concatenates path properly") {
      val ctx = buildCtx("root", Map("a" -> Map("b" -> 5)))
      val inner = IndexFn(GetFn("root", false, RootFn, "pos"), ConstantFn("a"), "pos")
      val outer = IndexFn(inner, ConstantFn("b"), "pos")

      for result <- outer.resolve(ctx)
        yield assertTrue(result._1 == Some(5))
    },

    // --------------------------------------------------------------------------
    // Complex chain: foo.do()[3][5]
    // --------------------------------------------------------------------------
    test("Fn-returning list receiver handles chained indexing") {
      // pretend foo.do() returns a list of lists
      val fnReturningList: Fn[Any] = ConstantFn(List(List(1,2,3), List(4,5,6)))
      val fn = IndexFn(IndexFn(fnReturningList, ConstantFn(1), "pos"), ConstantFn(2), "pos")
      val ctx = DynaContext(Map.empty, TestHelpers.emptyDL)

      for result <- fn.resolve(ctx)
        yield assertTrue(result._1 == Some(6))
    },

    // --------------------------------------------------------------------------
    // Optional propagation across multiple levels
    // --------------------------------------------------------------------------
    test("Nested Option[List] propagation: None stays None") {
      val ctx = buildCtx("root", Some(List(None, Some(2))), true)
      val fn = IndexFn(IndexFn(GetFn("root", true, RootFn, "pos"), ConstantFn(0), "pos"), ConstantFn(0), "pos")

      for result <- fn.resolve(ctx)
        yield assertTrue(result._1 == None)
    }
  ).provide(
    ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
  ) @@ ziotestkit