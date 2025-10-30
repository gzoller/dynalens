package co.blocke.dynalens
package fn

import zio.*
import zio.test.*
import co.blocke.testkit.ZioTestKit.*


object StringFnSpec extends ZIOSpecDefault:

  // ---------- Test model ----------
  case class Foo(
                  s: String,                // general string for transforms
                  t: String,                // another string (used in concat/replace)
                  name: String,             // for interpolate
                  xs: List[Int],            // not used but keeps Foo realistic
                  optS: Option[String]      // optional string for some cases
                )

  // ---------- Lenses / context ----------
  val dynalens = DynaLens.into[Foo]
  val fooLens  = dynalens.topLens
  val foo = Foo("  HeLlO  ", "Alpha beta", "World", List(1,2,3), None)
  def buildCtx = DynaContext(Map.empty, dynalens).bind("foo", foo, fooLens)

  // ---------- Short-hands ----------
  // Get symbol at a path off top-level context symbols
  def G(path: String): GetFn = GetFn(path, false, RootFn, "pos")
  // Constant of anything
  def C(v: Any): ConstantFn[Any] = ConstantFn(v)
  // Constant Boolean
  def B(b: Boolean): ConstantFn[Boolean] = ConstantFn(b)

  def spec =
    suite("StringFnSpec")(

      // -------------------------------
      // Trim / case transforms
      // -------------------------------
      test("TrimFn trims whitespace; lens propagated from recv") {
        val ctx = buildCtx
        val fn  = TrimFn(G("foo.s"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(
            exit == Exit.succeed(("HeLlO", fooLens.fields("s")))
          )
      },

      test("ToLowerFn lowercases; lens propagated from recv") {
        val ctx = buildCtx
        val fn  = ToLowerFn(G("foo.s"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(
            exit == Exit.succeed(("  hello  ".toLowerCase, fooLens.fields("s")))
          )
      },

      test("ToUpperFn uppercases; lens propagated from recv") {
        val ctx = buildCtx
        val fn  = ToUpperFn(G("foo.s"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(
            exit == Exit.succeed(("  HeLlO  ".toUpperCase, fooLens.fields("s")))
          )
      },

      test("TrimFn wrong recv type errors") {
        val ctx = DynaContext(Map.empty, TestHelpers.emptyDL).bind("x", 42, ScalarLens("x", false, None))
        val fn  = TrimFn(G("x"), "pos-x")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit.isFailure)
      },

      // -------------------------------
      // Concat
      // -------------------------------
      test("ConcatFn concatenates recv + args; lens from recv") {
        val ctx = buildCtx
        val fn  = ConcatFn(G("foo.t"), List(C(" :: "), C("tail")), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(
            exit == Exit.succeed(("Alpha beta :: tail", fooLens.fields("t")))
          )
      },

      test("ConcatFn flattens Options in args (None/Some(None) → \"\")") {
        val ctx = buildCtx
        val fn  = ConcatFn(G("foo.t"), List(C(None), C("!"), C(Some(None))), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(
            exit == Exit.succeed(("Alpha beta!", fooLens.fields("t")))
          )
      },

      test("ConcatFn coerces non-string recv via toString; lens from recv") {
        val ctx = DynaContext(Map.empty, TestHelpers.emptyDL).bind("x", 999, ScalarLens("x", false, None))
        val fn  = ConcatFn(G("x"), List(C("a")), "pos-x")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(
            exit == Exit.succeed(("999a", ScalarLens("x", false, None)))
          )
      },

      // -------------------------------
      // Replace
      // -------------------------------
      test("ReplaceFn replaces literal occurrences; lens from recv") {
        val ctx = buildCtx
        // replace "a" with "A" => "AlphA betA"
        val fn  = ReplaceFn(G("foo.t"), C("a"), C("A"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(
            exit == Exit.succeed(("AlphA betA", fooLens.fields("t")))
          )
      },

      test("ReplaceFn with Option replacement: None / Some(None) → remove matches") {
        val ctx = buildCtx
        // remove 'a' characters entirely
        val fn  = ReplaceFn(G("foo.t"), C("a"), C(None), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(
            exit == Exit.succeed(("Alph bet", fooLens.fields("t"))) // "Alpha beta" with 'a' removed -> "Alph bet"
          )
      },

      test("ReplaceFn wrong recv type errors") {
        val ctx = DynaContext(Map.empty, TestHelpers.emptyDL).bind("x", true, ScalarLens("x", false, None))
        val fn  = ReplaceFn(G("x"), C("true"), C("false"), "pos-x")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit.isFailure)
      },

      // -------------------------------
      // Interpolate
      // -------------------------------
      test("InterpolateFn fills {name} from ctx symbol; missing vars → \"\"") {
        val ctx = buildCtx
        val tmpl = "Hello {name}, {missing}!"
        val fn = InterpolateFn(C(tmpl), Map("name" -> G("foo.name")), "pos")
        // Expect: name=World, missing="" => "Hello World, !"
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(
            exit == Exit.succeed(("Hello World, !", ScalarLens("<const>", false, None)))
          )
      },

      // -------------------------------
      // Substring
      // -------------------------------
      test("SubstringFn(start, end) normal bounds") {
        val ctx = DynaContext(Map.empty, TestHelpers.emptyDL)
          .bind("s", "abcdef", ScalarLens("s", false, None))
        val fn  = SubstringFn(G("s"), C(1), Some(C(4)), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(
            exit == Exit.succeed(("bcd", ScalarLens("s", false, None)))
          )
      },

      test("SubstringFn with start beyond length → \"\"") {
        val ctx = DynaContext(Map.empty, TestHelpers.emptyDL)
          .bind("s", "abc", ScalarLens("s", false, None))
        val fn  = SubstringFn(G("s"), C(100), None, "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(
            exit == Exit.succeed(("", ScalarLens("s", false, None)))
          )
      },

      test("SubstringFn wrong recv type errors") {
        val ctx = DynaContext(Map.empty, TestHelpers.emptyDL).bind("n", 12345, ScalarLens("n", false, None))
        val fn  = SubstringFn(G("n"), C(0), Some(C(2)), "pos-n")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit.isFailure)
      },

      // -------------------------------
      // Lens propagation sanity
      // -------------------------------
      test("Lens from recv is returned by string transforms (e.g., TrimFn)") {
        val ctx = buildCtx
        val fn  = TrimFn(G("foo.s"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(
            exit == Exit.succeed(("HeLlO", fooLens.fields("s")))
          )
      }
    ).provide(
      ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
    ) @@ ziotestkit