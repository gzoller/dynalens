package co.blocke.dynalens
package fn

import zio.*
import zio.test.*
import co.blocke.testkit.ZioTestKit.*

object ArithmeticFnSpec extends ZIOSpecDefault {

  case class Foo(
                  i: Int,
                  l: Long,
                  f: Float,
                  d: Double,
                  bi: BigInt,
                  bd: BigDecimal,
                  s: String
                )

  val lensFoo = ClassLens(
    name = "foo",
    isOptional = false,
    parent = None,
    fields = Map(
      "i"  -> ScalarLens("i", false, None),
      "l"  -> ScalarLens("l", false, None),
      "f"  -> ScalarLens("f", false, None),
      "d"  -> ScalarLens("d", false, None),
      "bi" -> ScalarLens("bi", false, None),
      "bd" -> ScalarLens("bd", false, None),
      "s"  -> ScalarLens("s", false, None)
    ),
    _get = (f,obj) => ZIO.succeed(f match {
      case "i"  => obj.asInstanceOf[Foo].i
      case "l"  => obj.asInstanceOf[Foo].l
      case "f"  => obj.asInstanceOf[Foo].f
      case "d"  => obj.asInstanceOf[Foo].d
      case "bi" => obj.asInstanceOf[Foo].bi
      case "bd" => obj.asInstanceOf[Foo].bd
      case "s"  => obj.asInstanceOf[Foo].s
    }),
    _update = (_, newVal, obj) =>
      ZIO.succeed(obj.asInstanceOf[Foo].copy(s = newVal.asInstanceOf[String]))
  )

  val fooObj = Foo(2, 3L, 1.5f, 10.5, BigInt(100), BigDecimal(42.7), "hello")

  def buildCtx: DynaContext =
    DynaContext(
      Map(
        "top"  -> (fooObj, lensFoo),
        "this" -> (fooObj, lensFoo)
      )
    )

  def G(path: String): GetFn =
    GetFn(path, false, RootFn, "pos")

  override def spec = suite("ArithmeticFn") (

    // --------------------
    // NEGATE
    // --------------------
    test("Negate int → int") {
      val fn = NegateFn(G("i"), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == -2)
    },

    test("Negate string fails") {
      val fn = NegateFn(G("s"), "")
      for result <- fn.resolve(buildCtx).exit
        yield assertTrue(result.isFailure)
    },

    // --------------------
    // ADD
    // --------------------
    test("Add int + long → long result") {
      val fn = AddFn(G("i"), List(G("l")), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == 5L)
    },

    test("Add int + float → float result") {
      val fn = AddFn(G("i"), List(G("f")), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == 3.5f)
    },

    test("Add int + double → double result") {
      val fn = AddFn(G("i"), List(G("d")), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == 12.5)
    },

    test("Add int + BigInt → BigInt result") {
      val fn = AddFn(G("i"), List(G("bi")), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == BigInt(102))
    },

    test("Add int + BigDecimal → BigDecimal result") {
      val fn = AddFn(G("i"), List(G("bd")), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == BigDecimal(44.7))
    },

    test("Add string + int should fail") {
      val fn = AddFn(G("s"), List(G("i")), "")
      for result <- fn.resolve(buildCtx).exit
        yield assertTrue(result.isFailure)
    },


    // --------------------
    // SUBTRACT
    // --------------------
    test("Subtract double - int") {
      val fn = SubtractFn(G("d"), List(G("i")), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == 8.5)
    },

    test("Subtract BigInt - long") {
      val fn = SubtractFn(G("bi"), List(G("l")), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == BigInt(97))
    },

    test("Subtract string - int fails") {
      val fn = SubtractFn(G("s"), List(G("i")), "")
      for result <- fn.resolve(buildCtx).exit
        yield assertTrue(result.isFailure)
    },


    // --------------------
    // MULTIPLY
    // --------------------
    test("Multiply float * int") {
      val fn = MultiplyFn(G("f"), List(G("i")), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == 3.0f)
    },

    test("Multiply BigDecimal * long") {
      val fn = MultiplyFn(G("bd"), List(G("l")), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == BigDecimal(128.1))
    },


    // --------------------
    // DIVIDE
    // --------------------
    test("Divide double / float") {
      val fn = DivideFn(G("d"), List(G("f")), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(math.abs(value.asInstanceOf[Double] - (10.5/1.5)) < 0.0001)
    },

    test("Divide int / zero → error") {
      val fn = DivideFn(G("i"), List(ConstantFn(0)), "")
      for result <- fn.resolve(buildCtx).exit
        yield assertTrue(result.isFailure)
    },


    // --------------------
    // MODULO
    // --------------------
    test("Modulo long % int") {
      val fn = ModuloFn(G("l"), List(G("i")), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == 1L)
    },

    test("Modulo by zero → error") {
      val fn = ModuloFn(G("i"), List(ConstantFn(0)), "")
      for result <- fn.resolve(buildCtx).exit
        yield assertTrue(result.isFailure)
    },


    // --------------------
    // Lens Preservation
    // --------------------
    test("Left-hand operand lens preserved") {
      val fn = AddFn(G("l"), List(G("i")), "") // left = long
      for (_, lens) <- fn.resolve(buildCtx)
        yield assertTrue(lens.name == "l")
    },


    // --------------------
    // Optional lens behavior
    // --------------------
    test("Optional Some arithmetic works") {
      val ctx = buildCtx.bind("optI", Some(5), ScalarLens("optI", true, None))
      val fn = AddFn(GetFn("optI", true, RootFn, "pos"), List(ConstantFn(2)), "")
      for result <- fn.resolve(ctx).exit
        yield assertTrue(result.isFailure)
    },

    test("Optional None arithmetic returns None") {
      val ctx = buildCtx.bind("optI", None, ScalarLens("optI", true, None))
      val fn = AddFn(GetFn("optI", true, RootFn, "pos"), List(ConstantFn(2)), "")
      for result <- fn.resolve(ctx).exit
        yield assertTrue(result.isFailure)
    }

  ).provide(
    ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
  ) @@ ziotestkit
}