package co.blocke.dynalens
package fn

import zio.*
import zio.test.*
import co.blocke.testkit.ZioTestKit.*


object MiscFnSpec extends ZIOSpecDefault:

  case class Root(x: Int)
  val rootLens = DynaLens.into[Root].topLens

  def spec = suite("MiscFn Tests")(

    suite("ElseFn")(
      test("ElseFn passes through Some value") {
        val lens = ScalarLens("x", true, None)
        val ctx = DynaContext(Map.empty).bind("x", "good", lens)
        val fn = ElseFn(G("x"), C("bad"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit == Exit.succeed(("good", ScalarLens("x", true, None))))
      },

      test("ElseFn supplies default for None") {
        val lens = ScalarLens("x", true, None)
        val ctx = DynaContext(Map.empty).bind("x", None, lens)
        val fn = ElseFn(G("x"), C("fallback"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit == Exit.succeed(("fallback", lens)))
      },

      test("ElseFn supplies default for null") {
        val lens = ScalarLens("x", true, None)
        val ctx = DynaContext(Map.empty).bind("x", null, lens)
        val fn = ElseFn(G("x"), C("fallback"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit == Exit.succeed(("fallback", lens)))
      },

      test("ElseFn with FailSentinel on None triggers failure") {
        val ctx = DynaContext(Map.empty).bind("x", None, ScalarLens("x", true, None))
        val fn = ElseFn(G("x"), C(FailSentinel), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit.isFailure)
      },

      test("ElseFn with FailSentinel on null triggers failure") {
        val ctx = DynaContext(Map.empty).bind("x", null, ScalarLens("x", true, None))
        val fn = ElseFn(G("x"), C(FailSentinel), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit.isFailure)
      },

      test("ElseFn fails if receiver is non-Option (no recovery)") {
        val ctx = DynaContext(Map.empty).bind("x", 1234, ScalarLens("x", false, None))
        val fn = ElseFn(G("x"), C("unused"), "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit.isFailure)
      }
    ).provide(
      ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
    ) @@ ziotestkit,

    suite("BlockFn")(
      test("BlockFn executes statements and returns final Fn result") {
        val ctx = DynaContext(Map.empty).bind("a", 1, ScalarLens("a", false, None))
        val stmt = UpdateStmt("a", C(2), "pos")  // assume UpdateStmt exists and tested elsewhere
        val block = BlockFn(Seq(stmt), G("a"), "pos")
        for exit <- block.resolve(ctx).exit
          yield assertTrue(exit == Exit.succeed((2, ScalarLens("a", false, None))))
      },
      test("BlockFn threads context correctly through multiple statements") {
        val rootValue = Root(10)
        val ctx = DynaContext(
          Map(
            "x"   -> (10, rootLens.fields("x")),
            "top" -> (rootValue, rootLens)
          )
        )
        val stmt1 = UpdateStmt("x", ConstantFn(20), "pos")
        val block = BlockFn(Seq(stmt1), GetFn("x", false, RootFn, "pos"), "pos")

        for exit <- block.resolve(ctx).exit
          yield assertTrue(exit == Exit.succeed((20, ScalarLens("x", false, None))))
      }
    ).provide(
      ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
    ) @@ ziotestkit,

    suite("CaseWhenFn")(
      test("CaseWhenFn exact case match") {
        val ctx = DynaContext(Map.empty).bind("n", 2, ScalarLens("n", false, None))
        val cases = Vector(1 -> C("one"), 2 -> C("two"))
        val fn = CaseWhenFn(G("n"), cases, None, false, "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit == Exit.succeed(("two", ScalarLens("n", false, None))))
      },
      test("CaseWhenFn fall to default") {
        val ctx = DynaContext(Map.empty).bind("n", 3, ScalarLens("n", false, None))
        val fn = CaseWhenFn(G("n"), Vector(1 -> C("one")), Some(C("other")), false, "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit == Exit.succeed(("other", ScalarLens("n", false, None))))
      },
      test("CaseWhenFn permissive mode returns receiver when no match") {
        val ctx = DynaContext(Map.empty).bind("n", 9, ScalarLens("n", false, None))
        val fn = CaseWhenFn(G("n"), Vector(1 -> C("one")), None, permissive = true, "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit == Exit.succeed((9, ScalarLens("n", false, None))))
      },
      test("CaseWhenFn strict mode without default fails on no match") {
        val ctx = DynaContext(Map.empty).bind("n", 9, ScalarLens("n", false, None))
        val fn = CaseWhenFn(G("n"), Vector(1 -> C("one")), None, permissive = false, "pos")
        for exit <- fn.resolve(ctx).exit
          yield assertTrue(exit.isFailure)
      }
    ).provide(
      ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
    ) @@ ziotestkit,

    suite("UUIDFn")(
      test("UUIDFn generates value and lens") {
        val ctx = DynaContext(Map.empty)
        val fn = UUIDFn("pos")
        for exit <- fn.resolve(ctx).exit
          yield exit match
            case Exit.Success((uuid: String, lens)) =>
              assertTrue(uuid.nonEmpty && lens.name == "uuid")
            case _ => assertTrue(false)
      },
      test("UUIDFn always produces different UUIDs") {
        val ctx = DynaContext(Map.empty)
        val fn = UUIDFn("pos")
        for
          exit1 <- fn.resolve(ctx).exit
          exit2 <- fn.resolve(ctx).exit
        yield assertTrue(
          exit1 != exit2
        )
      }
    ).provide(
      ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
    ) @@ ziotestkit
  )