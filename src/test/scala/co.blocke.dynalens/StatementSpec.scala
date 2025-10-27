package co.blocke.dynalens

import zio.*
import zio.test.*
import co.blocke.testkit.ZioTestKit._
import fn.*
import zio.test.Assertion.*
import zio.test.ZIOSpecDefault


object StatementSpec extends ZIOSpecDefault:

  // --------------------------------------------------
  // Test model + lenses
  // --------------------------------------------------

  case class Foo(a: Int, b: Boolean)

  val fooLens: ClassLens =
    ClassLens(
      name = "Foo",
      isOptional = false,
      parent = None,
      fields = Map(
        "a" -> ScalarLens("a", false, None),
        "b" -> ScalarLens("b", false, None)
      ),
      _get = (fn, obj) =>
        obj.asInstanceOf[Foo] match
          case Foo(v, _) if fn == "a" => ZIO.succeed(v)
          case Foo(_, v) if fn == "b" => ZIO.succeed(v)
          case _ => ZIO.fail(DynaLensError("", s"No such field '$fn'")),
      _update = (field, value, obj) =>
        obj.asInstanceOf[Foo] match
          case Foo(a, b) if field == "a" =>
            ZIO.succeed(obj.asInstanceOf[Foo].copy(a = value.asInstanceOf[Int]))
          case Foo(a, b) if field == "b" =>
            ZIO.succeed(obj.asInstanceOf[Foo].copy(b = value.asInstanceOf[Boolean]))
          case _ =>
            ZIO.fail(DynaLensError("", s"No such field '$field'"))
    )

  val baseCtx: DynaContext =
    DynaContext(Map("top" -> (Foo(5, true), fooLens)))


  def G(path: String): GetFn = GetFn(path, false, RootFn, "")


  // --------------------------------------------------
  // Tests
  // --------------------------------------------------
  def spec = suite("StatementSpec") (

    // ----------------------------------------
    suite("ValStmt") (

      test("binds a new scalar value into context") {
        val stmt = ValStmt("x", ConstantFn(42))
        for
          ctx2 <- stmt.resolve(baseCtx)
        yield assert(ctx2.get("x").map(_._1))(isSome(equalTo(42)))
      },

      test("rebinds an existing symbol") {
        val ctx = baseCtx.bind("x", 10, fooLens)
        val stmt = ValStmt("x", ConstantFn(77))
        for
          ctx2 <- stmt.resolve(ctx)
        yield assert(ctx2.get("x").map(_._1))(isSome(equalTo(77)))
      },

      test("binds a Foo object with lens tracking") {
        val foo2 = Foo(9, false)
        val stmt = ValStmt("y", ConstantFn(foo2))
        for
          ctx2 <- stmt.resolve(baseCtx)
        yield assertTrue(
          ctx2.get("y").exists { case (v, lens) =>
            v == foo2 &&
              lens.isInstanceOf[ScalarLens] &&
              lens.name == "<const>"
          }
        )
      }
    ),


    // ----------------------------------------
    suite("IfStmt") (

      test("executes then block when condition true") {
        val stmt = IfStmt(
          condition = ConstantFn(true),
          thenBlock = ValStmt("x", ConstantFn(100))
        )
        for
          ctx2 <- stmt.resolve(baseCtx)
        yield assert(ctx2.get("x").map(_._1))(isSome(equalTo(100)))
      },

      test("executes else block when condition false") {
        val stmt = IfStmt(
          condition = ConstantFn(false),
          thenBlock = ValStmt("x", ConstantFn(10)),
          elseBlock = Some(ValStmt("x", ConstantFn(99)))
        )
        for
          ctx2 <- stmt.resolve(baseCtx)
        yield assert(ctx2.get("x").map(_._1))(isSome(equalTo(99)))
      },

      test("no else block keeps original context") {
        val stmt = IfStmt(ConstantFn(false), ValStmt("x", ConstantFn(1)))
        for
          ctx2 <- stmt.resolve(baseCtx)
        yield assert(ctx2.get("x"))(isNone)
      },

      test("nested if statements propagate context correctly") {
        val inner = IfStmt(ConstantFn(true), ValStmt("y", ConstantFn(11)))
        val outer = IfStmt(ConstantFn(true), inner)
        for
          ctx2 <- outer.resolve(baseCtx)
        yield assert(ctx2.get("y").map(_._1))(isSome(equalTo(11)))
      }
    ),


    // ----------------------------------------
    suite("BlockStmt") (

      test("executes statements in order") {
        val block = BlockStmt(Seq(
          ValStmt("x", ConstantFn(1)),
          ValStmt("x", ConstantFn(2))
        ))
        for
          ctx2 <- block.resolve(baseCtx)
        yield assert(ctx2.get("x").map(_._1))(isSome(equalTo(2)))
      },

      test("later statements see earlier results") {
        val block = BlockStmt(Seq(
          ValStmt("a", ConstantFn(5)),
          ValStmt("sum", AddFn(G("a"), List(ConstantFn(7).asInstanceOf[Fn[Any]]), "here"))
        ))
        for
          ctx2 <- block.resolve(baseCtx)
        yield assert(ctx2.get("sum").map(_._1))(isSome(equalTo(12)))
      },

      test("failure in block stops further execution") {
        val bad = ValStmt("oof", G("not_found")) // resolve error
        val block = BlockStmt(Seq(
          ValStmt("a", ConstantFn(5)),
          bad,
          ValStmt("b", ConstantFn(9)) // must NOT execute
        ))
        for
          exit <- block.resolve(baseCtx).exit
        yield assert(exit)(fails(anything))
      },

      test("nested blocks execute fully") {
        val nested = BlockStmt(Seq(
          ValStmt("x", ConstantFn(3)),
          BlockStmt(Seq(
            ValStmt("y", ConstantFn(8))
          ))
        ))
        for
          ctx2 <- nested.resolve(baseCtx)
        yield assertTrue(
          ctx2.get("x").map(_._1).contains(3),
          ctx2.get("y").map(_._1).contains(8)
        )
      }
    )
  ).provide(
    ZLayer.succeed(
      RuntimeEnv(
        biMapRegistry = new BiMapRegistry()
      )
    )
  ) @@ ziotestkit
