package co.blocke.dynalens
package fn

import zio.*
import zio.test.*
import co.blocke.testkit.ZioTestKit._


object GetFnSpec extends ZIOSpecDefault {

  //
  // Test Model
  //
  case class Address(street: String, city: String)
  case class Person(name: String, age: Int, address: Address)
  case class Item(name: String, qty: Int)
  case class Order(items: List[Item], attrs: Map[String, String])

  val lensAddress = ClassLens(
    name = "address",
    isOptional = false,
    parent = None,
    fields = Map(
      "street" -> ScalarLens("street", false, None),
      "city"   -> ScalarLens("city",   false, None)
    ),
    _get = (f,obj) => ZIO.succeed(f match {
      case "street" => obj.asInstanceOf[Address].street
      case "city"   => obj.asInstanceOf[Address].city
    }),
    _update = (_, _, obj) => ZIO.succeed(obj),
    schema = DynaLens.into[Address].schema
  )

  val lensPerson = ClassLens(
    name = "person",
    isOptional = false,
    parent = None,
    fields = Map(
      "name"    -> ScalarLens("name", false, None),
      "age"     -> ScalarLens("age", false, None),
      "address" -> lensAddress.copy(parent = None)
    ),
    _get = (f,obj) => ZIO.succeed(f match {
      case "name"    => obj.asInstanceOf[Person].name
      case "age"     => obj.asInstanceOf[Person].age
      case "address" => obj.asInstanceOf[Person].address
    }),
    _update = (_, _, obj) => ZIO.succeed(obj),
    schema = DynaLens.into[Person].schema
  )

  val personObj = Person("Greg", 51, Address("Main St", "Dallas"))
  val topLens = lensPerson.copy(parent = None)
  val dynalens = DynaLens(topLens, DynaLens.into[Person].schema, Map(
    "co.blocke.dynalens.Person" -> topLens,
    "co.blocke.dynalens.Address" -> lensAddress
  ))

  def buildCtx: DynaContext =
    DynaContext(
      symbols = Map(
        "top" -> (personObj, topLens),
        "this" -> (personObj, topLens) // default receiver fallback
      ),
      dynaLens = dynalens
    )

  val lensItem = ClassLens(
    name = "item",
    isOptional = false,
    parent = None,
    fields = Map(
      "name" -> ScalarLens("name", false, None),
      "qty" -> ScalarLens("qty", false, None)
    ),
    _get = (f, obj) => ZIO.succeed(f match {
      case "name" => obj.asInstanceOf[Item].name
      case "qty" => obj.asInstanceOf[Item].qty
    }),
    _update = (_, _, obj) => ZIO.succeed(obj),
    schema = DynaLens.into[Item].schema
  )

  val lensOrder = ClassLens(
    name = "order",
    isOptional = false,
    parent = None,
    fields = Map(
      "items" -> ListLens("items", false, lensItem, None),
      "attrs" -> MapLens("attrs", false, MapKeyKind.StringKey, ScalarLens("value", false, None), None)
    ),
    _get = (f, obj) => ZIO.succeed(f match {
      case "items" => obj.asInstanceOf[Order].items
      case "attrs" => obj.asInstanceOf[Order].attrs
    }),
    _update = (_, _, obj) => ZIO.succeed(obj),
    schema = DynaLens.into[Order].schema
  )

  val orderObj = Order(
    List(Item("A", 1), Item("B", 2), Item("C", 3)),
    Map("color" -> "red", "size" -> "large")
  )

  val orderCtx =
    buildCtx
      .bind("order", orderObj, lensOrder)

  // Distinct types for Phase 4 tests
  case class P4Item(label: String, price: Double)
  case class P4Order(items: List[P4Item], attrs: Map[String, String])

  val p4Order = P4Order(
    List(P4Item("A", 1.99), P4Item("B", 3.49), P4Item("C", 5.25)),
    Map("color" -> "red", "size" -> "XL")
  )

  val lensP4Item = ClassLens(
    name = "p4item",
    isOptional = false,
    parent = None,
    fields = Map(
      "label" -> ScalarLens("label", false, None),
      "price" -> ScalarLens("price", false, None)
    ),
    _get = (f, obj) => ZIO.succeed(f match {
      case "label" => obj.asInstanceOf[P4Item].label
      case "price" => obj.asInstanceOf[P4Item].price
    }),
    _update = (_, _, obj) => ZIO.succeed(obj),
    schema = DynaLens.into[P4Item].schema
  )

  val lensP4Order = ClassLens(
    name = "p4order",
    isOptional = false,
    parent = None,
    fields = Map(
      "items" -> ListLens("items", false, lensP4Item, None),
      "attrs" -> MapLens("attrs", false,
        MapKeyKind.StringKey,
        ScalarLens("value", false, None), None)
    ),
    _get = (f, obj) => ZIO.succeed(f match {
      case "items" => obj.asInstanceOf[P4Order].items
      case "attrs" => obj.asInstanceOf[P4Order].attrs
    }),
    _update = (_, _, obj) => ZIO.succeed(obj),
    schema = DynaLens.into[P4Order].schema
  )

  val ctxP4 =
    buildCtx.bind("order4", p4Order, lensP4Order)
      .copy(dynaLens = dynalens)


  override def spec = suite("GetFn") (

    // 1 — Top-level scalar field read
    test("Get name - direct from top") {
      val fn = GetFn("name", isOptional = false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(buildCtx)
      yield assertTrue(value == "Greg")
    },

    // 2 — Nested scalar field read
    test("Get address.city") {
      val fn = GetFn("address.city", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(buildCtx)
      yield assertTrue(value == "Dallas")
    },

    // 3 — Deep nesting 3+ levels
    test("Get deeply nested street path") {
      val fn = GetFn("address.street", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(buildCtx)
      yield assertTrue(value == "Main St")
    },

    // 4 — Scalar on non-optional parent
    test("Get age from non-optional parent succeeds") {
      val fn = GetFn("age", isOptional = false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(buildCtx)
      yield assertTrue(value == 51)
    },

    // 5 — Full class object read (address)
    test("Get address object") {
      val fn = GetFn("address", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(buildCtx)
      yield assertTrue(value == Address("Main St", "Dallas"))
    },

    // 6 — Symbol anchor read
    test("Get name via symbol ==top") {
      val ctx = buildCtx.bind("p", personObj, topLens)
      val fn = GetFn("p.name", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == "Greg")
    },

    // 7 — Symbol overshadowing this/top
    test("Symbol takes precedence over this/top") {
      val ctx = buildCtx.bind("name", "Bad!", ScalarLens("name", false, None))
      val fn = GetFn("name", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == "Bad!")
    },

    // 8 — this fallback when no explicit symbol found
    test("Falls back to this when symbol missing") {
      val fn = GetFn("age", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(buildCtx)
      yield assertTrue(value == 51)
    },

    // 9 — top fallback when neither symbol nor this available
    test("Falls back to top if this missing") {
      val ctx = DynaContext(Map("top" -> (personObj, topLens)), dynalens)
      val fn = GetFn("name", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == "Greg")
    },

    // 10 — No anchor → error non-optional
    test("No anchor found when optional=false triggers error") {
      val ctx = DynaContext(Map(), dynalens) // no top, no this, no symbols
      val fn = GetFn("name", false, RootFn, "pos")
      for
        result <- fn.resolve(ctx).exit
      yield assertTrue(result.isFailure)
    },

    // 11 — No anchor → None optional
    test("No anchor optional=true returns None") {
      val ctx = DynaContext(Map(), dynalens)
      val fn = GetFn("name", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == None)
    },

    // 12 — Field not present → error
    test("Non-existent field returns error") {
      val fn = GetFn("junkField", false, RootFn, "pos")
      for
        result <- fn.resolve(buildCtx).exit
      yield assertTrue(result.isFailure)
    },

    // --- Phase 2: Optional Behavior Tests ---

    // 13 — Optional parent Some → Some(x)
    test("Optional parent Some → Some(x)") {
      val ctx = buildCtx.bind("optP", Some(personObj), topLens.copy(isOptional = true))
      val fn = GetFn("optP.name", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == Some("Greg"))
    },

    // 14 — Optional parent None + optional=true → None
    test("Optional parent None yields None") {
      val ctx = buildCtx.bind("optP", None, topLens.copy(isOptional = true))
      val fn = GetFn("optP.name", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == None)
    },

    // 15 — Optional parent None + optional=false → error
    test("Optional parent None but required=false → error") {
      val ctx = buildCtx.bind("optP", None, topLens.copy(isOptional = true))
      val fn = GetFn("optP.name", false, RootFn, "pos")
      for
        result <- fn.resolve(ctx).exit
      yield assertTrue(result.isFailure)
    },

    // 16 — Optional final scalar Some → Some(x)
    test("Optional final scalar Some → Some(x)") {
      val ctx = buildCtx.bind("optName", Some("Greg"), ScalarLens("optName", true, None))
      val fn = GetFn("optName", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == Some("Greg"))
    },

    // 17 — Missing required final field → error
    test("Missing required final field → error") {
      val ctx = DynaContext(Map("top" -> (personObj, topLens)), dynalens)
      val fn = GetFn("middle.nope", true, RootFn, "pos") // bad path under optional
      for
        result <- fn.resolve(ctx).exit
      yield assertTrue(result.isFailure)
    },

    // 18 — Optional middle Some → propagate Some(final)
    test("Optional middle Some → propagate Some(final)") {
      val optAddr = Some(Address("Main St", "Dallas"))
      val ctx = buildCtx.bind("p2", Person("Bob", 33, optAddr.get), topLens)
      val fn = GetFn("p2.address.city", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == Some("Dallas"))
    },

    // 19 — Optional middle Some but final field missing → error
    test("Optional middle Some but final field missing → None") {
      val lensOptPerson = topLens.copy(isOptional = true)
      val ctx = DynaContext(Map("top" -> (Some(personObj), lensOptPerson)), dynalens)
      val fn = GetFn("address.zip", true, RootFn, "pos") // missing field
      for
        result <- fn.resolve(ctx).exit
      yield assertTrue(result.isFailure)
    },

    // 20 — Nested optional auto-flatten → Some(x)
    test("Nested optional auto-flatten → Some(x)") {
      val lensOptPerson = topLens.copy(isOptional = true)
      val ctx = DynaContext(Map("top" -> (Some(personObj), lensOptPerson)), dynalens)
      val fn = GetFn("address.city", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == Some("Dallas"))
    },

    // 21 — Null under optional yields None
    test("Null under optional yields None") {
      val badPerson = Person("Greg", 51, null.asInstanceOf[Address])
      val lensOptPerson = topLens.copy(isOptional = true)
      val ctx = DynaContext(Map("top" -> (badPerson, lensOptPerson)), dynalens)
      val fn = GetFn("address.city", true, RootFn, "pos")
      for
        result <- fn.resolve(ctx).exit
      yield assertTrue(result.isFailure)
    },

    // 22 — Null encountered but required → error
    test("Null encountered but required → error") {
      val badPerson = Person("Greg", 51, null.asInstanceOf[Address])
      val lensOptPerson = topLens.copy(isOptional = true)
      val ctx = DynaContext(Map("top" -> (badPerson, lensOptPerson)), dynalens)
      val fn = GetFn("address.city", false, RootFn, "pos")
      for
        result <- fn.resolve(ctx).exit
      yield assertTrue(result.isFailure)
    },

    // 23 — Non-optional List, valid index → raw value
    test("List index on non-optional list returns raw element") {
      val fn = GetFn("order.items[1].name", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(orderCtx)
      yield assertTrue(value == "B")
    },

    // 24 — Non-optional List OOB index → error
    test("List index OOB on required → error") {
      val fn = GetFn("order.items[99].name", false, RootFn, "pos")
      for
        result <- fn.resolve(orderCtx).exit
      yield assertTrue(result.isFailure)
    },

    // 25 — Optional List indexing success → Some(x)
    test("Optional list Some → Some(elem)") {
      val ctx = orderCtx.bind("optOrder", Some(orderObj), lensOrder.copy(isOptional = true))
      val fn = GetFn("optOrder.items[2].name", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == Some("C"))
    },

    // 26 — Optional List index OOB → None
    test("Optional list OOB yields None") {
      val ctx = orderCtx.bind("optOrder", Some(orderObj), lensOrder.copy(isOptional = true))
      val fn = GetFn("optOrder.items[7].name", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == None)
    },

    // 27 — Optional List underlying None → None
    test("Optional list None → None for index read") {
      val ctx = orderCtx.bind("optOrder", None, lensOrder.copy(isOptional = true))
      val fn = GetFn("optOrder.items[1].name", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == None)
    },

    // 28 — Optional List underlying None + required → error
    test("Optional list None + required index → error") {
      val ctx = orderCtx.bind("optOrder", None, lensOrder.copy(isOptional = true))
      val fn = GetFn("optOrder.items[1].name", false, RootFn, "pos")
      for
        result <- fn.resolve(ctx).exit
      yield assertTrue(result.isFailure)
    },

    // 29 — Map key success non-optional → raw scalar
    test("Map key lookup success returns raw value") {
      val fn = GetFn("order.attrs[color]", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(orderCtx)
      yield assertTrue(value == "red")
    },

    // 30 — Map key fail required → error
    test("Missing map key on required fails") {
      val fn = GetFn("order.attrs[missing]", false, RootFn, "pos")
      for
        result <- fn.resolve(orderCtx).exit
      yield assertTrue(result.isFailure)
    },

    // 31 — Map key success optional → Some(x)
    test("Optional map Some → Some(value)") {
      val ctx = orderCtx.bind("optOrder", Some(orderObj), lensOrder.copy(isOptional = true))
      val fn = GetFn("optOrder.attrs[color]", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == Some("red"))
    },

    // 32 — Map key missing optional → None
    test("Optional map key missing yields None") {
      val ctx = orderCtx.bind("optOrder", Some(orderObj), lensOrder.copy(isOptional = true))
      val fn = GetFn("optOrder.attrs[nope]", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == None)
    },

    // 33 — Optional map underlying None → None
    test("Optional map None → None for key lookup") {
      val ctx = orderCtx.bind("optOrder", None, lensOrder.copy(isOptional = true))
      val fn = GetFn("optOrder.attrs[color]", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == None)
    },

    // 34 — Optional map underlying None + required → error
    test("Optional map None + required key fails") {
      val ctx = orderCtx.bind("optOrder", None, lensOrder.copy(isOptional = true))
      val fn = GetFn("optOrder.attrs[color]", false, RootFn, "pos")
      for
        result <- fn.resolve(ctx).exit
      yield assertTrue(result.isFailure)
    },

    // 35 — Optional list under map value returns Some
    test("Map → Some(list) → index → Some value") {
      val newOrder = orderObj.copy(
        attrs = Map("nums" -> "unused") // placeholder; list lives elsewhere
      )
      val ctx = orderCtx
        .bind("optOrder", Some(newOrder), lensOrder.copy(isOptional = true))
        .bind("nums", List(10, 20, 30), ListLens("nums", false,
          ScalarLens("int", false, None), None))

      val fn = GetFn("nums[1]", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == Some(20))
    },

    // 36 — Complex nested optional: Some(Some) → Some(value)
    test("Nested optionals cascade Some") {
      val ctx =
        orderCtx.bind("optOrder", Some(Some(orderObj)),
          lensOrder.copy(isOptional = true))

      val fn = GetFn("optOrder.items[0].name", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == Some("A"))
    },

    // 37 — Null map value under optional behaves as None
    test("Null map value under optional yields None") {
      val orderWithNull = orderObj.copy(attrs = Map("color" -> null.asInstanceOf[String]))
      val ctx = orderCtx.bind("optOrder", Some(orderWithNull), lensOrder.copy(isOptional = true))
      val fn = GetFn("optOrder.attrs[color]", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == None)
    },

    // 38 — Invalid list index key (String where Int expected) → error
    test("String index on List required fails") {
      val fn = GetFn("order.items[abc].name", false, RootFn, "pos")
      for
        result <- fn.resolve(orderCtx).exit
      yield assertTrue(result.isFailure)
    },

    // 39 — Invalid list index key optional=true → None
    test("String index on List optional yields None") {
      val ctx = orderCtx.bind("optOrder", Some(orderObj), lensOrder.copy(isOptional = true))
      val fn = GetFn("optOrder.items[abc].name", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == None)
    },

    // 40 — Enum key lookup success optional → Some
    test("Enum-like Map key lookup optional Some(value)") {
      val ctx = orderCtx.bind("optOrder", Some(orderObj), lensOrder.copy(isOptional = true))
      val fn = GetFn("optOrder.attrs[color]", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == Some("red"))
    },

    // 41 — Map key present but value is missing → None (optional)
    test("Map value missing yields None under optional") {
      val orderMissingValue = orderObj.copy(attrs = Map("color" -> "red"))
      val ctx = orderCtx.bind("optOrder", Some(orderMissingValue), lensOrder.copy(isOptional = true))
      val fn = GetFn("optOrder.attrs[size]", true, RootFn, "pos") // key exists but size missing from Map
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == None)
    },

    // 42 — Nested optional flattening across map → list → item
    test("Map → optional → list index → optional flatten") {
      val newOrder = orderObj.copy(attrs = Map("nums" -> "unused"))
      val ctx = orderCtx
        .bind("optOrder", Some(Some(newOrder)), lensOrder.copy(isOptional = true))
        .bind("nums", List(Some("X"), None, Some("Y")),
          ListLens("nums", true, ScalarLens("str", false, None), None))

      val fn = GetFn("nums[2]", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == Some("Y"))
    },

    // 43 — Map path but not MapLens → error
    test("Indexing scalar as map key → error") {
      val ctx = buildCtx.bind("scalar", "oops", ScalarLens("scalar", false, None))
      val fn = GetFn("scalar[oops]", false, RootFn, "pos")
      for
        result <- fn.resolve(ctx).exit
      yield assertTrue(result.isFailure)
    },

    // 44 — Direct symbol resolution
    test("Direct symbol resolves correctly") {
      val ctx = buildCtx.bind("p", personObj, topLens)
      val fn = GetFn("p.age", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == 51)
    },

    // 45 — Symbol overrides this and top
    test("Symbol overrides this and top when same name exists") {
      val ctx = buildCtx
        .bind("name", "override", ScalarLens("name", false, None))
      val fn = GetFn("name", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == "override")
    },

    // 46 — this used when symbol missing
    test("Fallback to this when symbol missing") {
      val fn = GetFn("address.street", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(buildCtx)
      yield assertTrue(value == "Main St")
    },

    // 47 — top used when symbol and this missing
    test("Fallback to top when symbol and this missing") {
      val ctx = DynaContext(Map("top" -> (personObj, topLens)), dynalens)
      val fn = GetFn("address.city", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == "Dallas")
    },

    // 48 — No receivers available required → error
    test("No context/this/top with required → error") {
      val ctx = DynaContext(Map(), dynalens)
      val fn = GetFn("age", false, RootFn, "pos")
      for
        result <- fn.resolve(ctx).exit
      yield assertTrue(result.isFailure)
    },

    // 49 — No receivers available optional → None
    test("No context/this/top optional → None") {
      val ctx = DynaContext(Map(), dynalens)
      val fn = GetFn("age", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == None)
    },

    // 50 — Symbol referencing list resolves index
    test("Symbol referencing list with index works") {
      val ctx = orderCtx.bind("xs", orderObj.items, lensOrder.fields("items"))
      val fn = GetFn("xs[1].name", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == "B")
    },

    // 51 — Symbol referencing map resolves key
    test("Symbol referencing map resolves key") {
      val ctx = orderCtx.bind("m", orderObj.attrs, lensOrder.fields("attrs"))
      val fn = GetFn("m[size]", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == "large")
    },

    // 52 — Symbol-scoped fallback inside lists
    test("Inside list scope fallback to list element as this") {
      val ctx = orderCtx.setThis(orderObj.items(0), lensItem)
      val fn = GetFn("qty", false, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == 1)
    },

    // 53 — Optional symbol Some → Some(value)
    test("Optional symbol Some → Some(field)") {
      val ctx = buildCtx.bind("optP", Some(personObj), topLens.copy(isOptional = true))
      val fn = GetFn("optP.age", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == Some(51))
    },

    // 54 — Optional symbol None required → error
    test("Optional symbol None required → error") {
      val ctx = buildCtx.bind("optP", None, topLens.copy(isOptional = true))
      val fn = GetFn("optP.age", false, RootFn, "pos")
      for
        result <- fn.resolve(ctx).exit
      yield assertTrue(result.isFailure)
    },

    // 55 — Optional symbol None optional → None
    test("Optional symbol None optional → None") {
      val ctx = buildCtx.bind("optP", None, topLens.copy(isOptional = true))
      val fn = GetFn("optP.address.city", true, RootFn, "pos")
      for
        (value, _) <- fn.resolve(ctx)
      yield assertTrue(value == None)
    },

    // 56 — Symbol resolution delivers correct lens depth
    test("Symbol path returns proper lens for chaining operations") {
      val ctx = orderCtx.bind("x", orderObj.items, lensOrder.fields("items"))
      val fn = GetFn("x[0].qty", false, RootFn, "pos")
      for
        (_, lens) <- fn.resolve(ctx)
      yield assertTrue(lens.name == "qty")
    },

    // 57 — Symbol resolution error when symbol exists but lens missing
    test("Symbol missing lens should error unless optional") {
      val ctx = buildCtx.copy(symbols = Map("x" -> (42, null))) // invalid lens
      val fn = GetFn("x", false, RootFn, "pos")
      for
        result <- fn.resolve(ctx).exit
      yield assertTrue(result.isFailure)
    },

    // 58 — Symbol vs this precedence inside list scope
    test("List scope: bare field uses this, prefixed uses symbol") {
      // this → first element of items ("A", qty=1)
      // symbol "sym" → second element ("B", qty=2)

      val ctx = orderCtx
        .setThis(orderObj.items(0), lensItem)
        .bind("sym", orderObj.items(1), lensItem)

      // Bare name → resolves via this.qty = 1
      val fnThis = GetFn("qty", false, RootFn, "pos")

      // Symbol override → resolves via sym.qty = 2
      val fnSym = GetFn("sym.qty", false, RootFn, "pos")

      for {
        (vThis, _) <- fnThis.resolve(ctx)
        (vSym, _)  <- fnSym.resolve(ctx)
      } yield assertTrue(
        vThis == 1 &&
          vSym == 2
      )
    },
  ).provide(
    ZLayer.succeed(
      RuntimeEnv(
        biMapRegistry = new BiMapRegistry()
      )
    )
  ) @@ ziotestkit
}
