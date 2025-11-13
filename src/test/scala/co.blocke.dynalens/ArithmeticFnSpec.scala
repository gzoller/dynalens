package co.blocke.dynalens
package fn

import zio.*
import zio.test.*
import co.blocke.testkit.ZioTestKit.*
import co.blocke.dynalens.TestHelpers.*

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

  val dynalens = DynaLens.into[Foo]
  val fooSchema = dynalens.schema
  val lensFoo = classLensFor[Foo](
    name = "foo",
    fields = Map(
      "i"  -> ScalarLens("i", false, None),
      "l"  -> ScalarLens("l", false, None),
      "f"  -> ScalarLens("f", false, None),
      "d"  -> ScalarLens("d", false, None),
      "bi" -> ScalarLens("bi", false, None),
      "bd" -> ScalarLens("bd", false, None),
      "s"  -> ScalarLens("s", false, None)
    ),
    getFn = (f,obj) => ZIO.succeed(f match {
      case "i"  => obj.asInstanceOf[Foo].i
      case "l"  => obj.asInstanceOf[Foo].l
      case "f"  => obj.asInstanceOf[Foo].f
      case "d"  => obj.asInstanceOf[Foo].d
      case "bi" => obj.asInstanceOf[Foo].bi
      case "bd" => obj.asInstanceOf[Foo].bd
      case "s"  => obj.asInstanceOf[Foo].s
    }),
    updFn = (_, newVal, obj) =>
      ZIO.succeed(obj.asInstanceOf[Foo].copy(s = newVal.asInstanceOf[String])),
    schema = fooSchema
  )

  val fooObj = Foo(2, 3L, 1.5f, 10.5, BigInt(100), BigDecimal(42.7), "hello")

  def buildCtx: DynaContext =
    DynaContext(
      symbols = Map.empty,      // start with no pre-bound symbols
      dynaLens = dynalens,
      rootObj = fooObj,
      rootLens = lensFoo,
      parentOpt = None
    )

  def G(path: String): GetFn = {
    val fieldType = util.PathUtil.getPathType(path,fooSchema).getOrElse(ScalarType(path, "scala.Any", false))
    GetFn(path, false, RootFn, "pos", Some(fieldType))
  }

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
      val fn = AddFn(G("i"), G("l"), ScalarType("", "scala.Long", false), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == 5L)
    },

    test("Add int + float → float result") {
      val fn = AddFn(G("i"), G("f"), ScalarType("", "scala.Float", false), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == 3.5f)
    },

    test("Add int + double → double result") {
      val fn = AddFn(G("i"), G("d"), ScalarType("", "scala.Double", false), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == 12.5)
    },

    test("Add int + BigInt → BigInt result") {
      val fn = AddFn(G("i"), G("bi"), ScalarType("", "scala.BigInt", false), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == BigInt(102))
    },

    test("Add int + BigDecimal → BigDecimal result") {
      val fn = AddFn(G("i"), G("bd"), ScalarType("", "scala.BigDecimal", false), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == BigDecimal(44.7))
    },

    test("Add string + int should fail") {
      val fn = AddFn(G("s"), G("i"), ScalarType("", "java.lang.String", false), "")
      for result <- fn.resolve(buildCtx).exit
        yield assertTrue(result.isFailure)
    },


    // --------------------
    // SUBTRACT
    // --------------------
    test("Subtract double - int") {
      val fn = SubtractFn(G("d"), G("i"), ScalarType("", "scala.Double", false), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == 8.5)
    },

    test("Subtract BigInt - long") {
      val fn = SubtractFn(G("bi"), G("l"), ScalarType("", "scala.BigInt", false), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == BigInt(97))
    },

    test("Subtract string - int fails") {
      val fn = SubtractFn(G("s"), G("i"), ScalarType("", "java.lang.String", false), "")
      for result <- fn.resolve(buildCtx).exit
        yield assertTrue(result.isFailure)
    },


    // --------------------
    // MULTIPLY
    // --------------------
    test("Multiply float * int") {
      val fn = MultiplyFn(G("f"), G("i"), ScalarType("", "scala.Float", false), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == 3.0f)
    },

    test("Multiply BigDecimal * long") {
      val fn = MultiplyFn(G("bd"), G("l"), ScalarType("", "scala.BigDecimal", false), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == BigDecimal(128.1))
    },


    // --------------------
    // DIVIDE
    // --------------------
    test("Divide double / float") {
      val fn = DivideFn(G("d"), G("f"), ScalarType("", "scala.Double", false), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(math.abs(value.asInstanceOf[Double] - (10.5/1.5)) < 0.0001)
    },

    test("Divide int / zero → error") {
      val fn = DivideFn(G("i"), ConstantFn(0), ScalarType("", "scala.Int", false), "")
      for result <- fn.resolve(buildCtx).exit
        yield assertTrue(result.isFailure)
    },


    // --------------------
    // MODULO
    // --------------------
    test("Modulo long % int") {
      val fn = ModuloFn(G("l"), G("i"), ScalarType("", "scala.Long", false), "")
      for (value, _) <- fn.resolve(buildCtx)
        yield assertTrue(value == 1L)
    },

    test("Modulo by zero → error") {
      val fn = ModuloFn(G("i"), ConstantFn(0), ScalarType("", "scala.Int", false), "")
      for result <- fn.resolve(buildCtx).exit
        yield assertTrue(result.isFailure)
    },


    // --------------------
    // Lens Preservation
    // --------------------
    test("Left-hand operand lens preserved") {
      val fn = AddFn(G("l"), G("i"), ScalarType("", "scala.Long", false), "") // left = long
      for (_, lens) <- fn.resolve(buildCtx)
        yield assertTrue(lens.name == "l")
    },

  ).provide(
    ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
  ) @@ ziotestkit
}