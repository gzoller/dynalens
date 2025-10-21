package co.blocke.dynalens
package parser

import zio.*
import zio.test.*
import co.blocke.dynalens.fn.*
import cfn.*
import co.blocke.testkit.ZioTestKit._


object RhsTypeSpec extends ZIOSpecDefault:
  def spec = suite("RhsType Tests")(
    test("numeric addition with receiver") {
      val recv = ConstantReceiver(ScalarType("", "scala.Int"), ConstantFn(3))
      val args = List(ConstantFn(2.0).asInstanceOf[Fn[Any]])
      val ctx = ExprContext(
        scriptText = "3+2.0",
        schema = ClassType("Root", "Root", Nil, false)
      )
      val fn = CPlusFn.build(recv, args)(using ctx).toOption.get
      val result = Utility.rhsType(fn)(using ctx)

      assertTrue(result.exists(_.typeName == "scala.Double"))
    },
    test("string upper transform with val receiver") {
      // Pre-stage symbol table
      val nameField = ValType("name", ScalarType("name", "java.lang.String"), "java.lang.String")
      val ctx = ExprContext(
        scriptText =
          """val name = "greg"
            |name.toUpper()""".stripMargin,
        schema = ClassType("Root", "Root", Nil, false),
        symbols = List(Map("name" -> nameField))
      )

      // Simulate receiver = val 'name'
      val recv = NamedReceiver("name", nameField, GetFn("name", isOptional = false, NoOpFn, ctx.posStr))
      val args = Nil // No arguments for toUpper()

      val fn = CToUpperFn.build(recv, args)(using ctx).toOption.get
      val result = Utility.rhsType(fn)(using ctx)

      assertTrue(result.exists(_.typeName == "java.lang.String"))
    },
    test("predicate on 'this' numeric field in filter") {
      val listField = ListType("listOfInts", ScalarType("", "scala.Int"), "scala.List[Int]")
      val ctx = ExprContext("listOfInts.filter(this > 3)", ClassType("Root", "Root", List(listField), false))

      val parentRecv = NamedReceiver("listOfInts", listField, GetFn("listOfInts", false, NoOpFn, ctx.posStr))
      val elemRecv = ElementReceiver(parentRecv, listField.elementType)
      val args = List(ConstantFn(3).asInstanceOf[Fn[Any]])
      val fn = CGreaterThanFn.build(elemRecv, args)(using ctx).toOption.get
      val result = Utility.rhsType(fn)(using ctx)

      assertTrue(result.exists(_.typeName == "scala.Boolean"))
    },
    test("predicate using val symbol") {
      val ageField = ScalarType("age", "scala.Int")
      val limitField = ValType("limit", ScalarType("limit", "scala.Int"), "scala.Int")

      val ctx = ExprContext(
        "val limit = 10\nage < limit",
        schema = ClassType("Root", "Root", List(ageField), false),
        symbols = List(Map("limit" -> limitField))
      )

      val recv = NamedReceiver("age", ageField, GetFn("age", false, NoOpFn, ctx.posStr))
      val arg = GetFn("limit", isOptional = false, NoOpFn, ctx.posStr)
      val fn = CLessThanFn.build(recv, List(arg))(using ctx).toOption.get
      val result = Utility.rhsType(fn)(using ctx)

      assertTrue(result.exists(_.typeName == "scala.Boolean"))
    },
    test("indexed value access") {
      val listField = ListType("foo", ScalarType("", "scala.Int"), "scala.List[Int]")
      val ctx = ExprContext("foo[3]", ClassType("Root", "Root", List(listField), false))

      val recv = NamedReceiver("foo", listField, GetFn("foo", false, NoOpFn, ctx.posStr))
      val indexArg = ConstantFn(3).asInstanceOf[Fn[Any]]
      val fn = CIndexFn.build(recv, List(indexArg))(using ctx).toOption.get

      // Simulate what rhsType does: indexing into a list yields its element type receiver
      val elemRecv = ElementReceiver(recv, listField.elementType)
      val elemResult = Utility.rhsType(fn)(using ctx.copy(receiver = Some(elemRecv)))

      assertTrue(elemResult.exists(_.typeName == "scala.Int"))
    },
    test("optional list filter") {
      val listType = ListType("optList", ScalarType("num", "scala.Int"), "scala.collection.immutable.List", isOptional = true)
      val listField = listType.copy(isOptional = true)
      val ctx = ExprContext("optList.filter(_ < 3)", ClassType("Root", "Root", List(listField), false))

      val recv = NamedReceiver("optList", listField, GetFn("optList", false, NoOpFn, ctx.posStr))
      // If this filter applies to a list, build using the list element receiver
      val elemRecv = ElementReceiver(recv, listField.elementType)
      val rawPred = CLessThanFn.build(elemRecv, List(ConstantFn(3).asInstanceOf[Fn[Any]]))(using ctx).toOption.get
      val fn = CFilterFn.build(recv, List(rawPred.asInstanceOf[Fn[Any]]))(using ctx).toOption.get
      val result = Utility.rhsType(fn)(using ctx)
      assertTrue(result.exists(_.typeName.contains("List")))
    },
    test("deeply chained functions") {
      val nameField = ScalarType("name", "java.lang.String")
      val ctx = ExprContext("deep chain", ClassType("Root", "Root", List(nameField), false))
      val recv = NamedReceiver("name", nameField, GetFn("name", false, NoOpFn, ctx.posStr))

      val trimFn  = CTrimFn.build(recv, Nil)(using ctx).toOption.get
      val trimType       = Utility.rhsType(trimFn)(using ctx).getOrElse(ScalarType("", "java.lang.String"))
      val trimRecv       = MethodReceiver(recv, trimType, trimFn.asInstanceOf[Fn[Any]])

      val upperFn = CToUpperFn.build(trimRecv, Nil)(using ctx).toOption.get
      val upperType      = Utility.rhsType(upperFn)(using ctx).getOrElse(ScalarType("", "java.lang.String"))
      val upperRecv      = MethodReceiver(trimRecv, upperType, upperFn.asInstanceOf[Fn[Any]])

      val subFn   = CSubstringFn.build(upperRecv, List(ConstantFn(1), ConstantFn(3)))(using ctx).toOption.get
      val subType        = Utility.rhsType(subFn)(using ctx).getOrElse(ScalarType("", "java.lang.String"))
      val subRecv        = MethodReceiver(upperRecv, subType, subFn.asInstanceOf[Fn[Any]])

      val concatFn= CConcatFn.build(subRecv, List(ConstantFn("X")))(using ctx).toOption.get
      val result         = Utility.rhsType(concatFn)(using ctx)
      assertTrue(result.exists(_.typeName == "java.lang.String"))
    },
    test("symbol shadowing resolution") {
      // Script equivalent:
      // val x = 1
      // { val x = "str"; x.toUpper }
      val outerVal = ValType("x", ScalarType("x", "scala.Int"), "scala.Int")
      val innerVal = ValType("x", ScalarType("x", "java.lang.String"), "java.lang.String")
      val ctxOuter = ExprContext("val x = 1", ClassType("Root", "Root", Nil, false), List(Map("x" -> outerVal)))
      val ctxInner = ctxOuter.copy(symbols = Map("x" -> innerVal) :: ctxOuter.symbols)

      val recv = NamedReceiver("x", innerVal, GetFn("x", false, NoOpFn, ctxInner.posStr))
      val fn = CToUpperFn.build(recv, Nil)(using ctxInner).toOption.get
      val result = Utility.rhsType(fn)(using ctxInner)
      assertTrue(result.exists(_.typeName == "java.lang.String"))
    },
    test("map key value access via IndexFn") {
      val mapField = MapType(
        "fooMap",
        ScalarType("", "java.lang.String"),
        ScalarType("", "scala.Int"),
        "scala.Map[String,Int]"
      )
      val ctx = ExprContext("fooMap['bar']", ClassType("Root", "Root", List(mapField), false))
      val recv = NamedReceiver("fooMap", mapField, GetFn("fooMap", false, NoOpFn, ctx.posStr))
      val keyArg = ConstantFn("bar").asInstanceOf[Fn[Any]]
      val fn = CIndexFn.build(recv, List(keyArg))(using ctx).toOption.get
      val result = Utility.rhsType(fn)(using ctx)
      assertTrue(result.exists(ft => ft.typeName == "scala.Int" && ft.isOptional))
    },
    test("unknown function fallback") {
      val nameField = ScalarType("name", "java.lang.String")
      val ctx = ExprContext("name.unknownFn()", ClassType("Root", "Root", List(nameField)))

      val fakeFn = new Fn[Any] {
        override val methodName = "unknownFn"
        val recv = NoOpFn
        val args = Nil
        val posStr = ctx.posStr
        override def children = Nil
        def rebuild(k: List[Fn[?]]) = this
        def resolve(ctx: DynaContext) = ???
      }
      val result = Utility.rhsType(fakeFn)(using ctx)
      assertTrue(result.isEmpty)
    },
    test("CaseWhenFn with homogeneous branches") {
      val recv = ConstantFn(2).asInstanceOf[Fn[Any]]
      val ctx = ExprContext("CASE WHEN 1 THEN 10 WHEN 2 THEN 20 ELSE 0", ClassType("Root", "Root", Nil, false))

      val cases = Vector(1 -> ConstantFn(10).asInstanceOf[Fn[Any]], 2 -> ConstantFn(20).asInstanceOf[Fn[Any]])
      val default = Some(ConstantFn(0).asInstanceOf[Fn[Any]])
      val fn = CaseWhenFn(recv, cases, default, false, ctx.posStr)

      val result = Utility.rhsType(fn)(using ctx)
      assertTrue(result.exists(_.typeName == "scala.Int"))
    },
    test("CaseWhenFn with mixed branch types") {
      val recv = ConstantFn("x").asInstanceOf[Fn[Any]]
      val ctx = ExprContext("CASE WHEN 'x' THEN 1 WHEN 'y' THEN 'str' ELSE true", ClassType("Root", "Root", Nil, false))

      val cases = Vector(
        "x" -> ConstantFn(1).asInstanceOf[Fn[Any]],
        "y" -> ConstantFn("str").asInstanceOf[Fn[Any]]
      )
      val default = Some(ConstantFn(true).asInstanceOf[Fn[Any]])
      val fn = CaseWhenFn(recv, cases, default, false, ctx.posStr)

      val result = Utility.rhsType(fn)(using ctx)
      // depending on your design this may be None or scala.Any
      assertTrue(result.isEmpty || result.exists(_.typeName == "scala.Any"))
    },
    test("Option[List] chained reverse().len()") {
      // Equivalent script: myOptDoubleList.reverse().len()
      val innerType = ScalarType("", "scala.Double")
      val listField = ListType("myOptDoubleList", innerType, "scala.List[Double]", isOptional = true)
      val ctx = ExprContext("myOptDoubleList.reverse().len()", ClassType("Root", "Root", List(listField), false))

      val recv = NamedReceiver("myOptDoubleList", listField, GetFn("myOptDoubleList", false, NoOpFn, ctx.posStr))

      // Apply reverse() which unwraps Option[List] to List
      val revFn = CReverseFn.build(recv, Nil)(using ctx).toOption.get
      val revType = Utility.rhsType(revFn)(using ctx).getOrElse(ScalarType("", "scala.Any"))
      val revRecv = MethodReceiver(recv, revType, revFn.asInstanceOf[Fn[Any]])

      // Then len() on the reversed list, which should re-wrap Option[Int]
      val lenFn = CLenFn.build(revRecv, Nil)(using ctx).toOption.get
      val result = Utility.rhsType(lenFn)(using ctx)
      assertTrue(result.exists(ft => ft.typeName == "scala.Int" && ft.isOptional))
    },
    test("cross-type numeric promotion") {
      val recv = ConstantReceiver(ScalarType("", "scala.Int"), ConstantFn(1))
      val args = List(ConstantFn(BigDecimal(2.5)).asInstanceOf[Fn[Any]])
      val ctx = ExprContext("1 + 2.5", ClassType("Root", "Root", Nil, false))
      val fn = CPlusFn.build(recv, args)(using ctx).toOption.get
      val result = Utility.rhsType(fn)(using ctx)
      assertTrue(result.exists(_.typeName == "scala.math.BigDecimal"))
    },
    test("nested Option[Map] and List indexing") {
      val innerType = ScalarType("", "scala.Int")
      val mapType = MapType("map", ScalarType("", "java.lang.String"), ListType("", innerType, "scala.List[Int]"), "scala.Map[String,List[Int]]")
      val optMap = mapType.copy(fieldName = "optMap", isOptional = true)
      val ctx = ExprContext("optMap['foo'][1]", ClassType("Root", "Root", List(optMap), false))

      val recv = NamedReceiver("optMap", optMap, GetFn("optMap", false, NoOpFn, ctx.posStr))
      val keyArg = ConstantFn("foo").asInstanceOf[Fn[Any]]

      // First: index into the map
      val firstIndexFn = CIndexFn.build(recv, List(keyArg))(using ctx).toOption.get
      val firstType = Utility.rhsType(firstIndexFn)(using ctx).getOrElse(ScalarType("", "scala.Any"))
      val firstRecv = MethodReceiver(recv, firstType, firstIndexFn.asInstanceOf[Fn[Any]])

      // Second: index into the resulting list
      val secondIndexFn = CIndexFn.build(firstRecv, List(ConstantFn(1).asInstanceOf[Fn[Any]]))(using ctx).toOption.get
      val result = Utility.rhsType(secondIndexFn)(using ctx)

      assertTrue(result.exists(ft => ft.typeName == "scala.Int" && ft.isOptional))
    },
    test("BlockFn returns last expression type") {
      val ctx = ExprContext("block test", ClassType("Root", "Root", Nil, false))
      // Simulate prior statements with dummy Statement instances
      val stmts = List(
        ValStmt("x", ConstantFn(3)),
        ValStmt("y", ConstantFn("middle"))
      )
      val block = BlockFn(
        stmts,
        ConstantFn(true),
        ctx.posStr
      )
      val result = Utility.rhsType(block)(using ctx)
      assertTrue(result.exists(_.typeName == "scala.Boolean"))
    },
    test("ConstantFn(null) yields scala.Null or scala.Any") {
      val ctx = ExprContext("null constant", ClassType("Root", "Root", Nil, false))
      val fn = ConstantFn(null).asInstanceOf[Fn[Any]]
      val result = Utility.rhsType(fn)(using ctx)
      assertTrue(result.exists(t => t.typeName == "scala.Null" || t.typeName == "scala.Any"))
    },
    test("type mismatch in relational comparison") {
      val strField = ScalarType("str", "java.lang.String")
      val ctx = ExprContext("str < 5", ClassType("Root", "Root", List(strField), false))
      val recv = NamedReceiver("str", strField, GetFn("str", false, NoOpFn, ctx.posStr))
      val args = List(ConstantFn(5).asInstanceOf[Fn[Any]])
      assertTrue(CLessThanFn.accepts(recv)(using ctx))
      val fn = CLessThanFn.build(recv, args)(using ctx).toOption.get
      val validation = CLessThanFn.validate(fn)(using ctx)
      assertTrue(
        validation.isLeft &&
        validation.left.get.msg.contains("requires operands to be mutually comparable types")
      )
    },
    test("map() works for List") {
      // Schema includes nums: List[Int]
      val listField = ListType("nums", ScalarType("", "scala.Int"), "scala.collection.immutable.List", false)
      val schema = ClassType("Root", "Root", List(listField), false)
      val ctx = ExprContext("nums => ...", schema)

      // Receiver for the list
      val recv = NamedReceiver("nums", listField, GetFn("nums", false, NoOpFn, ctx.posStr))

      // Transform returns a string value
      val transform = ConstantFn("stringValue").asInstanceOf[Fn[Any]]

      val result = Utility.rhsType(MapFn(recv.fn, transform, ctx.posStr))(using ctx)

      assertTrue(result.exists {
        case ListType(_, elem, _, _) => elem.typeName == "java.lang.String"
        case _ => false
      })
    },
    test("map() works for Option[List]") {
      val recvType =
        ListType("", ScalarType("", "scala.Int"), "scala.collection.immutable.List", isOptional = true)
      // Add "optNums" field to the schema so GetFn can resolve it
      val schema = ClassType("Root", "Root", List(recvType.copy(fieldName = "optNums")), false)
      val ctx = ExprContext("optNums.map(...)", schema)

      // Receiver for optNums
      val recv = NamedReceiver("optNums", recvType, GetFn("optNums", false, NoOpFn, ctx.posStr))

      val transform = ConstantFn(1.23).asInstanceOf[Fn[Any]]

      val result = Utility.rhsType(MapFn(recv.fn, transform, ctx.posStr))(using ctx)

      assertTrue(result.exists {
        case lt: ListType if lt.isOptional => lt.elementType.typeName == "scala.Double"
        case _ => false
      })
    },
    test("map() works for Map") {
      // Define Map[String,Int] field
      val mapField = MapType(
        "ages",
        ScalarType("", "scala.String"),
        ScalarType("", "scala.Int"),
        "scala.collection.immutable.Map"
      )

      // Compiler would know 'ages' is a field on Root
      val schema = ClassType("Root", "Root", List(mapField), false)
      val ctx = ExprContext("ages.map(...)", schema)

      // Receiver for the map
      val recv = NamedReceiver("ages", mapField, GetFn("ages", false, NoOpFn, ctx.posStr))
      val transform = ConstantFn(1.23).asInstanceOf[Fn[Any]]

      val result = Utility.rhsType(MapFn(recv.fn, transform, ctx.posStr))(using ctx)

      assertTrue(result.exists {
        case ListType(_, elem, _, _) =>
          elem.typeName == "scala.Double"
        case _ => false
      })
    },
    test("map() works for Option[Map]") {
      // Define Option[Map[String,Int]] field
      val recvType = MapType(
        "optAges",
        ScalarType("", "scala.String"),
        ScalarType("", "scala.Int"),
        "scala.collection.immutable.Map",
        isOptional = true
      )

      // Compiler would know 'optAges' is a field on Root
      val schema = ClassType("Root", "Root", List(recvType), false)
      val ctx = ExprContext("optAges.map(...)", schema)

      // Receiver for optAges
      val recv = NamedReceiver("optAges", recvType, GetFn("optAges", false, NoOpFn, ctx.posStr))
      val transform = ConstantFn(true).asInstanceOf[Fn[Any]]

      val result = Utility.rhsType(MapFn(recv.fn, transform, ctx.posStr))(using ctx)

      assertTrue(result.exists {
        case lt: ListType if lt.isOptional =>
          lt.elementType.typeName == "scala.Boolean"
        case _ => false
      })
    }
  ) @@ ziotestkit
