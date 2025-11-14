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

  case class Wrapper(foo: Foo)
  val wrapper = Wrapper(foo)
  val wrapperDL = DynaLens.into[Wrapper]
  val wrapperLens = wrapperDL.topLens

  def buildCtx(values: (String, Any)*): DynaContext =
    val symbols = values.map { case (k, v) => k -> (v, ScalarLens(k, false, None)) }.toMap
    val rootKey = values.headOption.map(_._1).getOrElse("root")
    val rootVal = values.headOption.map(_._2).getOrElse(null)
    val rootLens = ScalarLens(rootKey, false, None)
    DynaContext(
      symbols = symbols,
      dynaLens = TestHelpers.emptyDL,
      rootObj = rootVal,
      rootLens = rootLens
    )

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
    ) @@ziotestkit,

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
    ) @@ziotestkit,

    suite("Lens propagation")(
      test("Lens from recv is returned") {
        val ctx = DynaContext(
          symbols = Map.empty,
          dynaLens = wrapperDL,
          rootObj = wrapper,
          rootLens = wrapperLens
        )
        val fn  = LessThanFn(G("foo.a"), C(10), "pos")

        for exit <- fn.resolve(ctx).exit yield {
          exit match
            case Exit.Success((_, lens)) =>
              println("---HERE---")
              val cond1 = lens.name == "a"
              val cond2 = classOf[ScalarLens].isAssignableFrom(lens.getClass) // restore strict type check
              val cond3 = lens.parent.exists(_.name == "foo")

              println(
                s"""\nLens debug:
                   |  cond1 (name == "a"):         %s (actual: %s)
                   |  cond2 (is ScalarLens):       %s (class: %s)
                   |  cond3 (parent.name == foo):  %s (parent: %s)
                   |  lens loader:                 %s
                   |  ScalarLens loader:           %s
                   |""".stripMargin.format(
                  cond1, lens.name,
                  cond2, lens.getClass,
                  cond3, lens.parent.map(_.name),
                  lens.getClass.getClassLoader,
                  classOf[ScalarLens].getClassLoader
                )
              )

              assert(cond1)(Assertion.isTrue) &&
              assert(cond2)(Assertion.isTrue) &&
              assert(cond3)(Assertion.isTrue)
            case Exit.Failure(cause) =>
              println("---FAIL CAUSE---")
              println(cause.prettyPrint)
              assertTrue(false)
            case _ =>
              assertTrue(false)
        }
      },

      test("Lens from recv via GetFn resolves") {
        val ctx = DynaContext(
          symbols = Map.empty,
          dynaLens = wrapperDL,
          rootObj = wrapper,
          rootLens = wrapperLens
        )
        val get = G("foo.a")
        for exit <- get.resolve(ctx).exit yield {
          exit match
            case Exit.Success((_, lens)) =>
              val cond1 = lens.name == "a"
              val cond2 = classOf[ScalarLens].isAssignableFrom(lens.getClass)
              val cond3 = lens.parent.exists(_.name == "foo")
              println(
                s"""\nLens(GetFn) debug:
                   |  cond1 (name == "a"):         %s (actual: %s)
                   |  cond2 (is ScalarLens):       %s (class: %s)
                   |  cond3 (parent.name == foo):  %s (parent: %s)
                   |  lens loader:                 %s
                   |  ScalarLens loader:           %s
                   |""".stripMargin.format(
                  cond1, lens.name,
                  cond2, lens.getClass,
                  cond3, lens.parent.map(_.name),
                  lens.getClass.getClassLoader,
                  classOf[ScalarLens].getClassLoader
                )
              )
              assertTrue(cond1 && cond2 && cond3)
            case Exit.Failure(cause) =>
              println("---GETFN FAIL CAUSE---")
              println(cause.prettyPrint)
              assertTrue(false)
            case _ =>
              assertTrue(false)
        }
      }
    ).provide(
      ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
    ) @@ ziotestkit
  )