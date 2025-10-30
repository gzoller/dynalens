package co.blocke.dynalens
package fn

import zio.*
import zio.test.*
import co.blocke.testkit.ZioTestKit.*


object BooleanFnSpec extends ZIOSpecDefault:

  case class Foo(s: String, n: Int, flag: Boolean, opt: Option[Int], items: List[Int])
  val foo = Foo("Hello world", 42, true, Some(9), List(1,2,3))

  val dynalens = DynaLens.into[Foo]
  val fooLens = dynalens.topLens
  def buildCtx =
    DynaContext(
      Map("foo" -> (foo, fooLens)),
      dynalens
    )

  def G(path: String): GetFn = GetFn(path, false, RootFn, "pos")
  def C(v: Any): ConstantFn[Any] = ConstantFn(v)
  def B(v: Boolean): ConstantFn[Any] = ConstantFn(v)

  def Bad: Fn[Any] = new UnaryFn[Any]:
    override val recv: Fn[Any] = RootFn
    override val posStr: String = "bad"

    override def args: List[Fn[Any]] = Nil

    // Must use wildcard type (?) to match abstract signature
    override def rebuild(kids: List[Fn[?]]): Fn[Any] = this

    override def resolve(ctx: DynaContext) =
      ZIO.fail(DynaLensError("", "BadFn invoked"))

  override def spec = suite("BooleanFnSpec")(

    // --------------------
    // IF
    // --------------------
    test("IfFn true path") {
      val fn = IfFn(AndFn(G("foo.flag"), List(B(true)), ""), C("yes"), C("no"), "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(v == "yes")
    },

    test("IfFn false path") {
      val fn = IfFn(AndFn(G("foo.flag"), List(B(false)), ""), C("yes"), C("no"), "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(v == "no")
    },

    // --------------------
    // AND
    // --------------------
    test("AndFn both true") {
      val fn = AndFn(G("foo.flag"), List(B(true)), "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(v)
    },

    test("AndFn short-circuits false") {
      val fn = AndFn(B(false), List(Bad), "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(!v)
    },

    test("AndFn invalid left type errors") {
      val fn = AndFn(G("foo.s"), List(B(true)), "")
      for result <- fn.resolve(buildCtx).exit
        yield assertTrue(result.isFailure)
    },

    // --------------------
    // OR
    // --------------------
    test("OrFn both false") {
      val fn = OrFn(B(false), List(B(false)), "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(!v)
    },

    test("OrFn short-circuits true") {
      val fn = OrFn(B(true), List(Bad), "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(v)
    },

    test("OrFn invalid right type errors") {
      val fn = OrFn(B(false), List(G("foo.n")), "")
      for result <- fn.resolve(buildCtx).exit
        yield assertTrue(result.isFailure)
    },

    // --------------------
    // NOT
    // --------------------
    test("NotFn negates boolean") {
      val fn = NotFn(G("foo.flag"), "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(!v)
    },

    test("NotFn error on non-boolean") {
      val fn = NotFn(G("foo.s"), "")
      for result <- fn.resolve(buildCtx).exit
        yield assertTrue(result.isFailure)
    },

    // --------------------
    // ISDEFINED
    // --------------------
    test("IsDefinedFn Some true") {
      val fn = IsDefinedFn(G("foo.opt"), "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(v)
    },

    test("IsDefinedFn None false") {
      val ctx = buildCtx.copy(symbols = buildCtx.symbols.updated("foo", (foo.copy(opt=None), fooLens)))
      val fn = IsDefinedFn(GetFn("foo.opt", true, RootFn, "pos"), "")
      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(!v)
    },

    test("IsDefinedFn empty List false") {
      val ctx = buildCtx.copy(symbols = buildCtx.symbols.updated("foo", (foo.copy(items=Nil), fooLens)))
      val fn = IsDefinedFn(G("foo.items"), "")
      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(!v)
    },

    // --------------------
    // STARTSWITH / ENDSWITH
    // --------------------
    test("StartsWithFn success") {
      val fn = StartsWithFn(G("foo.s"), C("Hello"), "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(v)
    },

    test("EndsWithFn success") {
      val fn = EndsWithFn(G("foo.s"), C("world"), "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(v)
    },

    // --------------------
    // CONTAINS
    // --------------------
    test("Contains string substring") {
      val fn = ContainsFn(G("foo.s"), C("world"), "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(v)
    },

    test("Contains list value") {
      val fn = ContainsFn(G("foo.items"), C(2), "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(v)
    },

    test("Contains list predicate (>=2)") {
      val pred = GreaterThanFn(G("this"), C(1), "")
      val fn = ContainsFn(G("foo.items"), pred.asInstanceOf[Fn[Any]], "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(v)
    },

    test("Contains map key") {
      val ctx = DynaContext(Map.empty, TestHelpers.emptyDL).bind("m", Map("a" -> 1, "b" -> 2), ScalarLens("<const>", false, None))
      val fn = ContainsFn(G("m"), C("a"), "")
      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v)
    },

    test("Contains unsupported type errors") {
      val fn = ContainsFn(G("foo.n"), C(1), "")
      for result <- fn.resolve(buildCtx).exit
        yield assertTrue(result.isFailure)
    },

    // --------------------
    // EqualsIgnoreCase
    // --------------------
    test("EqualsIgnoreCaseFn match") {
      val fn = EqualsIgnoreCaseFn(G("foo.s"), C("hello WORLD"), "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(v)
    },

    // --------------------
    // MatchesRegex
    // --------------------
    test("MatchesRegexFn success") {
      val fn = MatchesRegexFn(G("foo.s"), C("Hello.*"), "")
      for (v, _) <- fn.resolve(buildCtx)
        yield assertTrue(v)
    }
  ).provide(
    ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
  ) @@ ziotestkit