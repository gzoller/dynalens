package co.blocke.dynalens

import zio.*
import zio.test.*
import parser.*
import co.blocke.dynalens.fn.*
import parser.cfn.*
import co.blocke.dynalens._BiMapRegistry
import co.blocke.testkit.ZioTestKit._


object RhsTypeConsistencySpec extends ZIOSpecDefault:

  def spec = suite("RhsType ↔ Runtime Consistency Tests")(

    // ─────────────────────────────────────────────
    test("Numeric addition: inferred type matches runtime result") {
      val recv = ConstantReceiver(ScalarType("", "scala.Int"), ConstantFn(2))
      val args = List(ConstantFn(3.5).asInstanceOf[Fn[Any]])
      val ctx  = ExprContext("2 + 3.5", ClassType("Root", "Root", Nil))
      val fn = CPlusFn.build(recv, args)(using ctx).toOption.get

      val inferredType = Utility.rhsType(fn)(using ctx).map(_.typeName)
      val runtimeValue = Unsafe.unsafe { implicit u =>
        Runtime.default.unsafe.run(fn.resolve(DynaContext.empty).provide(BiMapRegistry.empty)).getOrThrow()
      }

      val runtimeType = runtimeValue.getClass.getName
      val normalizedRuntime = runtimeType.replace("java.lang.", "scala.")

      println(s"[Numeric Add] inferred=$inferredType runtime=$runtimeType normalized=$normalizedRuntime")

      assertTrue(inferredType.exists(t =>
        normalizedRuntime.contains(t) || t.contains(normalizedRuntime)
      ))
    },

    // ─────────────────────────────────────────────
    test("String transform: inferred type matches runtime result") {
      val recv = ConstantReceiver(ScalarType("", "java.lang.String"), ConstantFn("greg"))
      val ctx  = ExprContext("\"greg\".toUpper()", ClassType("Root", "Root", Nil))
      val fn = CToUpperFn.build(recv, Nil)(using ctx).toOption.get

      val inferredType = Utility.rhsType(fn)(using ctx).map(_.typeName)
      val runtimeValue = Unsafe.unsafe { implicit u =>
        Runtime.default.unsafe.run(fn.resolve(DynaContext.empty).provide(BiMapRegistry.empty)).getOrThrow()
      }

      val runtimeType = runtimeValue.getClass.getName
      println(s"[ToUpper] inferred=$inferredType runtime=$runtimeType")
      assertTrue(inferredType.exists(runtimeType.contains))
    },

    // ─────────────────────────────────────────────
    // ─────────────────────────────────────────────
    test("Collection map(): inferred Option[List] type matches runtime") {
      val recvType = ListType(
        "optNums",
        ScalarType("", "scala.Int"),
        "scala.collection.immutable.List",
        true
      )
      val schema = ClassType("Root", "Root", List(recvType))
      val ctx    = ExprContext("optNums.map(_ * 2)", schema)

      // This mirrors what the compiler would emit for a field reference
      val recv = NamedReceiver("optNums", recvType, GetFn("optNums", false, NoOpFn, ctx.posStr))

      // The function argument (map transform)
      val transform = ConstantFn(1.23).asInstanceOf[Fn[Any]]

      val fn = MapFn(recv.fn, transform, ctx.posStr)

      // Compile-time inferred type
      val inferredType = Utility.rhsType(fn)(using ctx).map(_.typeName)

      // Runtime context simulating the object being evaluated
      val dctx = DynaContext(Map("optNums" -> Some(List(1, 2, 3))), None)
      val runtimeValue = Unsafe.unsafe { implicit u =>
        Runtime.default.unsafe.run(fn.resolve(dctx).provide(BiMapRegistry.layer(EmptyBiMapRegistry))).getOrThrow()
      }

      val runtimeType = runtimeValue.getClass.getName
      val normalizedRuntime =
        runtimeType
          .replace("scala.Some", "scala.Option")
          .replace("java.util.Optional", "scala.Option")
          .replace("scala.collection.immutable.$colon$colon", "scala.collection.immutable.List")

      println(s"[Option[List] map] inferred=$inferredType runtime=$runtimeType normalized=$normalizedRuntime")
      assertTrue(
        inferredType.exists(t =>
          normalizedRuntime.contains(t) || t.contains(normalizedRuntime)
        )
      )
    },

    // ─────────────────────────────────────────────
    test("Map transform: inferred Map type matches runtime result") {
      val mapType = MapType(
        "ages",
        ScalarType("", "java.lang.String"),
        ScalarType("", "scala.Int"),
        "scala.collection.immutable.Map"
      )
      val schema = ClassType("Root", "Root", List(mapType))
      val ctx    = ExprContext("ages.map(...)", schema)
      val recv = NamedReceiver("ages", mapType, GetFn("ages", false, RootFn, ctx.posStr))
      val transform = ConstantFn(1.23).asInstanceOf[Fn[Any]]
      val fn = MapFn(recv.fn, transform, ctx.posStr)

      val inferredType = Utility.rhsType(fn)(using ctx).map(_.typeName)
      val dctx = DynaContext(Map("ages" -> Map("a" -> 10, "b" -> 20)), None)
      val runtimeValue = Unsafe.unsafe { implicit u =>
        Runtime.default.unsafe.run(fn.resolve(dctx).provide(BiMapRegistry.empty)).getOrThrow()
      }

      val runtimeType = runtimeValue.getClass.getName
      val normalizedRuntime =
        runtimeType
          .replace("scala.collection.immutable.$colon$colon", "scala.collection.immutable.List")
          .replace("scala.Some", "scala.Option")
          .replace("scala.collection.immutable.Map$Map2", "scala.collection.immutable.Map")
      println(s"[Map map] inferred=$inferredType runtime=$normalizedRuntime")
      assertTrue(inferredType.exists(normalizedRuntime.contains))
    },
  ) @@ ziotestkit