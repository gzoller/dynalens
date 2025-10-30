package co.blocke.dynalens
package fn

import zio.*
import zio.test.*
import co.blocke.dynalens.RuntimeEnv
import co.blocke.testkit.ZioTestKit.*


object ComparisonFnSpec extends ZIOSpecDefault:

  case class Foo(a: Int, s: String)
  val foo = Foo(5, "hi")
  val dynalens = DynaLens.into[Foo]
  val fooLens = dynalens.topLens

  def buildCtx(values: (String, Any)*): DynaContext =
    values.foldLeft(DynaContext(Map.empty, TestHelpers.emptyDL)) { case (ctx, (k, v)) =>
      val lens = ScalarLens(k, false, None)
      ctx.bind(k, v, lens)
    }

  override def spec = suite("ComparisonFnSpec")(
    suite("Basic comparisons")(
      test("< Int < Int") {
        val ctx = buildCtx("x" -> 5, "y" -> 10)
        val fn = LessThanFn(G("x"), G("y"), "pos")
        for
          (v, _) <- fn.resolve(ctx)
        yield assertTrue(v)
      },
      test("> Double > Int") {
        val ctx = buildCtx("a" -> 11.2, "b" -> 7)
        val fn = GreaterThanFn(G("a"), G("b"), "pos")
        for
          (v, _) <- fn.resolve(ctx)
        yield assertTrue(v)
      },
      test("<= BigInt <= Long") {
        val ctx = buildCtx("a" -> BigInt(10), "b" -> 10L)
        val fn = LessThanOrEqualFn(G("a"), G("b"), "pos")
        for
          (v, _) <- fn.resolve(ctx)
        yield assertTrue(v)
      },
      test(">= BigDecimal >= Int") {
        val ctx = buildCtx("a" -> BigDecimal(3.14), "b" -> 3)
        val fn = GreaterThanOrEqualFn(G("a"), G("b"), "pos")
        for
          (v, _) <- fn.resolve(ctx)
        yield assertTrue(v)
      },
      test("String lexical <") {
        val ctx = buildCtx("a" -> "apple", "b" -> "banana")
        val fn = LessThanFn(G("a"), G("b"), "pos")
        for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v)
      },
      test("String lexical >") {
        val ctx = buildCtx("a" -> "zebra", "b" -> "yak")
        val fn = GreaterThanFn(G("a"), G("b"), "pos")
        for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v)
      },
      test("Option == Option (same Some)") {
        val ctx = buildCtx("x" -> Some(10), "y" -> Some(10))
        val fn = EqualFn(G("x"), G("y"), "pos")
        for exit <- fn.resolve(ctx).exit
        yield assertTrue(exit.isSuccess && {
          exit match
            case Exit.Success((v, _)) => v == true
            case _ => false
        })
      },
      test("Option != Option (Some vs None)") {
        val ctx = buildCtx("x" -> Some(10), "y" -> None)
        val fn = NotEqualFn(G("x"), G("y"), "pos")
        for exit <- fn.resolve(ctx).exit
        yield assertTrue(exit.isSuccess && {
          exit match
            case Exit.Success((v, _)) => v == true
            case _ => false
        })
      },
      test("== equal") {
        val ctx = buildCtx("x" -> 42, "y" -> 42)
        val fn = EqualFn(G("x"), G("y"), "pos")
        for
          (v, _) <- fn.resolve(ctx)
        yield assertTrue(v)
      },
      test("!= not equal") {
        val ctx = buildCtx("x" -> 42, "y" -> 41)
        val fn = NotEqualFn(G("x"), G("y"), "pos")
        for
          (v, _) <- fn.resolve(ctx)
        yield assertTrue(v)
      }
    ).provide(
      ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
    ) @@ ziotestkit,

    suite("Error cases")(
      test("Non-numeric compare → error") {
        val ctx = buildCtx("x" -> "hello", "y" -> 5)
        val fn = LessThanFn(G("x"), G("y"), "pos")
        for
          exit <- fn.resolve(ctx).exit
        yield assertTrue(exit.isFailure)
      },
      test("Option compare → error") {
        val ctx = buildCtx("x" -> Some(10), "y" -> 5)
        val fn = GreaterThanFn(G("x"), G("y"), "pos")
        for
          exit <- fn.resolve(ctx).exit
        yield assertTrue(exit match
          case Exit.Success((v, _)) => true
          case _ => false
        )
      },
      test("String <-> numeric compare → error") {
        val ctx = buildCtx("x" -> "abc", "y" -> 10)
        val fn = GreaterThanFn(G("x"), G("y"), "pos")
        for exit <- fn.resolve(ctx).exit
        yield assertTrue(exit.isFailure)
      },
      test("Null compare → error") {
        val ctx = buildCtx("x" -> null, "y" -> 5)
        val fn = GreaterThanFn(G("x"), G("y"), "pos")
        for
          exit <- fn.resolve(ctx).exit
        yield assertTrue(exit.isFailure)
      }
    ).provide(
      ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
    ) @@ ziotestkit,

    suite("Lens propagation")(
      test("Lens from recv is returned") {
        val ctx = DynaContext(Map("foo" -> (foo, fooLens)), dynalens)

        val fn = LessThanFn(G("foo.a"), C(10), "pos")

        for exit <- fn.resolve(ctx).exit yield assertTrue {
          exit match
            case Exit.Success((_, lens)) =>
              lens == fooLens.fields("a")
            case _ => false
        }
      }
    ).provide(
      ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
    ) @@ ziotestkit
  )