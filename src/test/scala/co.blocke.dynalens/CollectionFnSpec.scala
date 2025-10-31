package co.blocke.dynalens
package fn

import zio.*
import zio.test.*
import co.blocke.testkit.ZioTestKit.*


object CollectionFnSpec extends ZIOSpecDefault {

  case class Foo(a: Int, b: String)

  private def buildCtx(rootName: String, data: Any): DynaContext = {
    var chosenDL: Option[DynaLens[?]] = None
    val lens: Lens =
      data match
        case l: List[?] =>
          l.headOption match
            case Some(_: Foo) =>
              val fooDl = DynaLens.into[Foo]
              chosenDL = Some(fooDl)
              ListLens(
                name = rootName,
                isOptional = false,
                elementLens = fooDl.topLens.asInstanceOf[ClassLens],
                parent = Some(fooDl.topLens)
              )
            case _ =>
              ListLens(rootName, false, ScalarLens("elem", false, None), None)

        case m: Map[?, ?] =>
          val hasFoo = m.values.headOption.exists(_.isInstanceOf[Foo])
          if (hasFoo) {
            val fooDl = DynaLens.into[Foo]
            chosenDL = Some(fooDl)
            MapLens(rootName, false, MapKeyKind.StringKey, fooDl.topLens, Some(fooDl.topLens))
          } else {
            // assume string-key map for now
            MapLens(rootName, false, MapKeyKind.StringKey, ScalarLens("value", false, None), None)
          }

        case _: String =>
          ScalarLens(rootName, false, None)

        case None =>
          // treat as optional list with unknown element type
          ListLens(rootName, true, ScalarLens("elem", true, None), None)

        case n: Int =>
          ScalarLens(rootName, false, None)

        case other =>
          throw RuntimeException(s"Unhandled type in test helper: ${other.getClass.getName}")

    DynaContext(Map(rootName -> (data, lens)), chosenDL.getOrElse(TestHelpers.emptyDL))
  }

  // Helpers for Option[List[_]] and Option[Map[_, _]] roots
  def buildOptListCtx(rootName: String, dataOpt: Option[List[Any]]): DynaContext = {
    val lens = ListLens(rootName, true, ScalarLens("elem", false, None), None)
    DynaContext(Map(rootName -> (dataOpt, lens)), TestHelpers.emptyDL)
  }

  def buildOptMapCtx(rootName: String, dataOpt: Option[Map[Any, Any]]): DynaContext = {
    val lens = MapLens(rootName, true, MapKeyKind.StringKey, ScalarLens("value", false, None), None)
    DynaContext(Map(rootName -> (dataOpt, lens)), TestHelpers.emptyDL)
  }

  // --- Real-lens builders for Foo collections ---
  def buildFooListCtx(rootName: String, data: List[Foo]): DynaContext = {
    val dl = DynaLens.into[Foo]
    val lens = ListLens(rootName, false, dl.topLens.asInstanceOf[ClassLens], Some(dl.topLens))
    DynaContext(Map(rootName -> (data, lens)), dl)
  }

  def buildOptFooListCtx(rootName: String, dataOpt: Option[List[Foo]]): DynaContext = {
    val dl = DynaLens.into[Foo]
    val lens = ListLens(rootName, true, dl.topLens.asInstanceOf[ClassLens], Some(dl.topLens))
    DynaContext(Map(rootName -> (dataOpt, lens)), dl)
  }

  def buildFooMapCtx(rootName: String, data: Map[String, Foo]): DynaContext = {
    val dl = DynaLens.into[Foo]
    val lens = MapLens(rootName, false, MapKeyKind.StringKey, dl.topLens, Some(dl.topLens))
    DynaContext(Map(rootName -> (data, lens)), dl)
  }

  def buildOptFooMapCtx(rootName: String, dataOpt: Option[Map[String, Foo]]): DynaContext = {
    val dl = DynaLens.into[Foo]
    val lens = MapLens(rootName, true, MapKeyKind.StringKey, dl.topLens, Some(dl.topLens))
    DynaContext(Map(rootName -> (dataOpt, lens)), dl)
  }

  def spec = suite("CollectionFn Tests") (

    /*───────────────────────────*
     * keys()
     *───────────────────────────*/
    test("keys() extracts keys from Map") {
      val data = Map("a" -> 1, "b" -> 2)
      val ctx = buildCtx("root", data)
      val fn = KeysFn(GetFn("root", false, RootFn, "pos"), "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == List("a", "b"))
    },

    /*───────────────────────────*
     * Phase 2A: Wrong receiver errors
     *───────────────────────────*/
    test("keys() on List should fail") {
      val ctx = buildCtx("root", List(1,2))
      val fn = KeysFn(GetFn("root", false, RootFn, "pos"), "pos")
      for result <- fn.resolve(ctx).either
      yield assertTrue(result.isLeft)
    },

    test("keys() on String should fail") {
      val ctx = buildCtx("root", "oops")
      val fn = KeysFn(GetFn("root", false, RootFn, "pos"), "pos")
      for result <- fn.resolve(ctx).either
      yield assertTrue(result.isLeft)
    },

    test("values() on List should fail") {
      val ctx = buildCtx("root", List(1,2))
      val fn = ValuesFn(GetFn("root", false, RootFn, "pos"), "pos")
      for result <- fn.resolve(ctx).either
      yield assertTrue(result.isLeft)
    },

    test("values() on String should fail") {
      val ctx = buildCtx("root", "hey")
      val fn = ValuesFn(GetFn("root", false, RootFn, "pos"), "pos")
      for result <- fn.resolve(ctx).either
      yield assertTrue(result.isLeft)
    },

    test("MapFn: scalar receiver should fail") {
      val ctx = buildCtx("root", 10)
      val pred = AddFn(GetFn("this", false, RootFn, "pos"), List(ConstantFn(1)), "pos")
      val fn   = MapFn(GetFn("root", false, RootFn, "pos"), pred, "pos")
      for result <- fn.resolve(ctx).either
      yield assertTrue(result.isLeft)
    },

    test("MapFn: string receiver should fail") {
      val ctx = buildCtx("root", "nope")
      val pred = AddFn(GetFn("this", false, RootFn, "pos"), List(ConstantFn(1)), "pos")
      val fn   = MapFn(GetFn("root", false, RootFn, "pos"), pred, "pos")
      for result <- fn.resolve(ctx).either
      yield assertTrue(result.isLeft)
    },

    test("keys() on None returns Nil") {
      val ctx = buildCtx("root", None)
      val fn = KeysFn(GetFn("root", true, RootFn, "pos"), "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == Nil)
    },

    /*───────────────────────────*
     * values()
     *───────────────────────────*/
    test("values() extracts values") {
      val data = Map("a" -> 1, "b" -> 2)
      val ctx = buildCtx("root", data)
      val fn = ValuesFn(GetFn("root", false, RootFn, "pos"), "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == List(1, 2))
    },

    test("values() on None yields Nil") {
      val ctx = buildCtx("root", None)
      val fn = ValuesFn(GetFn("root", true, RootFn, "pos"), "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == Nil)
    },

    /*───────────────────────────*
     * len()
     *───────────────────────────*/
    test("len() on List gives size") {
      val data = List(1, 2, 3)
      val ctx = buildCtx("root", data)
      val fn = LenFn(GetFn("root", false, RootFn, "pos"), "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == 3)
    },

    test("len() on Map gives size") {
      val data = Map("a" -> 10, "b" -> 20)
      val ctx = buildCtx("root", data)
      val fn = LenFn(GetFn("root", false, RootFn, "pos"), "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == 2)
    },

    test("len() on String gives length") {
      val ctx = buildCtx("root", "hey")
      val fn = LenFn(GetFn("root", false, RootFn, "pos"), "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == 3)
    },

    test("len() on None yields 0") {
      val ctx = buildCtx("root", None)
      val fn = LenFn(GetFn("root", true, RootFn, "pos"), "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == 0)
    },

    /*───────────────────────────*
     * limit()
     *───────────────────────────*/
    test("limit() truncates the list") {
      val ctx = buildCtx("root", List(1, 2, 3, 4))
      val fn = LimitFn(GetFn("root", false, RootFn, "pos"), 2, "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == List(1, 2))
    },

    test("limit() on None returns Nil") {
      val ctx = buildCtx("root", None)
      val fn = LimitFn(GetFn("root", true, RootFn, "pos"), 3, "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == Nil)
    },

    /*───────────────────────────*
     * reverse()
     *───────────────────────────*/
    test("reverse() reverses list") {
      val ctx = buildCtx("root", List(3, 2, 1))
      val fn = ReverseFn(GetFn("root", false, RootFn, "pos"), "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == List(1, 2, 3))
    },

    /*───────────────────────────*
     * distinct()
     *───────────────────────────*/
    test("distinct() removes duplicates") {
      val ctx = buildCtx("root", List("a", "b", "a", "b"))
      val fn = DistinctFn(GetFn("root", false, RootFn, "pos"), None, "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v.toSet == Set("a", "b"))
    },

    /*───────────────────────────*
     * clean()
     *───────────────────────────*/
    test("clean() removes nulls and None") {
      val ctx = buildCtx("root", List("x", None, "y", null))
      val fn = CleanFn(GetFn("root", false, RootFn, "pos"), "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == List("x", "y"))
    },

    /*───────────────────────────*
     * List[T]  →  Scalar   => List[U]
     *───────────────────────────*/
    test("MapFn: list → list via scalar predicate") {
      val ctx = buildCtx("root", List(1, 2))
      val pred = AddFn(GetFn("this", false, RootFn, "pos"), List(ConstantFn(1)), "pos")
      val fn   = MapFn(GetFn("root", false, RootFn, "pos"), pred, "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == List(2, 3))
    },

    /*───────────────────────────*
     * List[T]  →  Tuple2[K,V]   => Map[K,V]
     *───────────────────────────*/
    test("MapFn: list → map via tuple2 predicate") {
      val ctx = buildCtx("root", List(1, 2))
      val pred = Tuple2Fn(
        GetFn("this", false, RootFn, "pos"),
        List(AddFn(GetFn("this", false, RootFn, "pos"), List(ConstantFn(1)), "pos")),
        "pos"
      )
      val fn = MapFn(GetFn("root", false, RootFn, "pos"), pred, "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == Map(1 -> 2, 2 -> 3))
    },

    /*───────────────────────────*
     * Map[K,V] →  Scalar        => List[U]
     *───────────────────────────*/
    test("MapFn: map → list via scalar predicate on value") {
      val ctx = buildCtx("root", Map("a" -> 1, "b" -> 2))
      val pred = AddFn(GetFn("this_value", false, NoOpFn, "pos"), List(ConstantFn(1)), "pos")
      val fn   = MapFn(GetFn("root", false, RootFn, "pos"), pred, "pos")

      for (v, _) <- fn.resolve(ctx)
          list = v.asInstanceOf[List[Any]]
      yield assertTrue(list.toSet == Set(2, 3)) // order not guaranteed
    },

    /*───────────────────────────*
     * Map[K,V] →  Tuple2[K2,V2] => Map[K2,V2]
     *───────────────────────────*/
    test("MapFn: map → map via tuple2 predicate (key passthrough, value +10)") {
      val ctx = buildCtx("root", Map("a" -> 1, "b" -> 2))
      val pred = Tuple2Fn(
        GetFn("this_key", false, RootFn, "pos"),
        List(AddFn(GetFn("this_value", false, RootFn, "pos"), List(ConstantFn(10)), "pos")),
        "pos"
      )
      val fn = MapFn(GetFn("root", false, RootFn, "pos"), pred, "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == Map("a" -> 11, "b" -> 12))
    },

    /*───────────────────────────*
     * Option[List[T]] → Tuple2[K,V] => Option[Map[K,V]] (value-wise Map, lens optional)
     *───────────────────────────*/
    test("MapFn: Option[List] (None) → tuple2 → Map ⇒ value Map.empty (lens optional)") {
      val ctx = buildOptListCtx("root", None)
      val pred = Tuple2Fn(
        GetFn("this", false, RootFn, "pos"),
        List(AddFn(GetFn("this", false, RootFn, "pos"), List(ConstantFn(1)), "pos")),
        "pos"
      )
      val fn = MapFn(GetFn("root", true, RootFn, "pos"), pred, "pos")

      for (v, l) <- fn.resolve(ctx)
        yield assertTrue(v == Map.empty && l.isOptional)
    },

    /*───────────────────────────*
     * Option[Map[K,V]] → Scalar => Option[List[U]] (value-wise List, lens optional)
     *───────────────────────────*/
    test("MapFn: Option[Map] (None) → scalar → List ⇒ value Nil (lens optional)") {
      val ctx = buildOptMapCtx("root", None)
      val pred = AddFn(GetFn("this_value", false, RootFn, "pos"), List(ConstantFn(1)), "pos")
      val fn   = MapFn(GetFn("root", true, RootFn, "pos"), pred, "pos")

      for (v, l) <- fn.resolve(ctx)
        yield assertTrue(v == Nil && l.isOptional)
    },

    /*───────────────────────────*
     * Phase 3: Map/List transformations with case-class fields
     *───────────────────────────*/
    test("MapFn: map[F] → list via field access this_value.a") {
      val data = Map("a" -> Foo(1, "x"), "b" -> Foo(2, "y"))
      val ctx = buildFooMapCtx("root", data)
      val pred = AddFn(GetFn("this_value.a", false, RootFn, "pos"), List(ConstantFn(5)), "pos")
      val fn = MapFn(GetFn("root", false, RootFn, "pos"), pred, "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v.asInstanceOf[List[Any]].toSet == Set(6, 7))
    },

    test("MapFn: map[F] → map via tuple2, newKey = len(key), newValue = foo.a + 10") {
      val data = Map("ab" -> Foo(1, "x"), "c" -> Foo(2, "y"))
      val ctx = buildFooMapCtx("root", data)
      val pred = Tuple2Fn(
        LenFn(GetFn("this_key", false, RootFn, "pos"), "pos").asInstanceOf[Fn[Any]],
        List(AddFn(GetFn("this_value.a", false, RootFn, "pos"), List(ConstantFn(10)), "pos")),
        "pos"
      )
      val fn = MapFn(GetFn("root", false, RootFn, "pos"), pred, "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == Map(2 -> 11, 1 -> 12))
    },

    test("MapFn: list[F] → list via field this.a") {
      val data = List(Foo(1,"x"), Foo(2,"y"))
      val ctx = buildFooListCtx("root", data)
      val pred = AddFn(GetFn("this.a", false, RootFn, "pos"), List(ConstantFn(3)), "pos")
      val fn = MapFn(GetFn("root", false, RootFn, "pos"), pred, "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == List(4, 5))
    },

    test("MapFn: list[F] → map via tuple2 (a as key, b as value)") {
      val data = List(Foo(1,"x"), Foo(2,"y"))
      val ctx = buildFooListCtx("root", data)
      val pred = Tuple2Fn(
        GetFn("this.a", false, RootFn, "pos"),
        List(GetFn("this.b", false, RootFn, "pos")),
        "pos"
      )
      val fn = MapFn(GetFn("root", false, RootFn, "pos"), pred, "pos")

      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == Map(1 -> "x", 2 -> "y"))
    },

    test("MapFn: Option[Map[K,F]] Some → map transform preserves optional lens") {
      val dataOpt = Some(Map("a" -> Foo(1,"x")))
      val ctx = buildOptFooMapCtx("root", dataOpt)
      val pred = AddFn(GetFn("this_value.a", false, RootFn, "pos"), List(ConstantFn(10)), "pos")
      val fn = MapFn(GetFn("root", true, RootFn, "pos"), pred, "pos")

      for (v, l) <- fn.resolve(ctx)
        yield assertTrue(v == List(11) && l.isOptional)
    },

    test("MapFn: Option[Map[K,F]] None → Nil, lens optional") {
      val ctx = buildOptFooMapCtx("root", None)
      val pred = AddFn(GetFn("this_value.a", false, RootFn, "pos"), List(ConstantFn(10)), "pos")
      val fn = MapFn(GetFn("root", true, RootFn, "pos"), pred, "pos")

      for (v, l) <- fn.resolve(ctx)
        yield assertTrue(v == Nil && l.isOptional)
    },

    test("MapFn: Option[List[F]] Some → list transform preserves optional lens") {
      val dataOpt = Some(List(Foo(1,"z")))
      val ctx = buildOptFooListCtx("root", dataOpt)
      val pred = AddFn(GetFn("this.a", false, RootFn, "pos"), List(ConstantFn(5)), "pos")
      val fn = MapFn(GetFn("root", true, RootFn, "pos"), pred, "pos")

      for (v, l) <- fn.resolve(ctx)
        yield assertTrue(v == List(6) && l.isOptional)
    },

    test("MapFn: Option[List[F]] None → Nil, lens optional") {
      val ctx = buildOptFooListCtx("root", None)
      val pred = AddFn(GetFn("this.a", false, RootFn, "pos"), List(ConstantFn(3)), "pos")
      val fn = MapFn(GetFn("root", true, RootFn, "pos"), pred, "pos")

      for (v, l) <- fn.resolve(ctx)
        yield assertTrue(v == Nil && l.isOptional)
    },

    test("MapFn should NOT attempt ClassLens upgrade for primitive list elements") {
      val data = List(1, 2, 3)
      val ctx = buildCtx("root", data)
      val pred = AddFn(GetFn("this", false, RootFn, "pos"), List(ConstantFn(1)), "pos")
      val fn = MapFn(GetFn("root", false, RootFn, "pos"), pred, "pos")

      for (v, l) <- fn.resolve(ctx)
        yield assertTrue(
          v == List(2, 3, 4) &&
            l.isInstanceOf[ListLens]
        )
    },

    test("List receiver with tuple-producing predicate yields Map; last-write-wins on dup keys") {
      val data = List("ab", "c", "ab")
      val ctx  = buildCtx("root", data)
      val recv = GetFn("root", false, RootFn, "pos")
      val len  = LenFn(GetFn("this", false, RootFn, "(this)"), "(len)")
      val pred = Tuple2Fn(len.asInstanceOf[Fn[Any]], List(ConstantFn(1)), "(tuple)")
      val m    = MapFn(recv, pred, "(=>)")

      for (out, _) <- m.resolve(ctx)
        yield assertTrue(out == Map(2 -> 1, 1 -> 1))
    },

    test("List receiver with non-tuple predicate yields List") {
      val data = List(1,2,3)
      val ctx  = buildCtx("root", data)
      val recv = GetFn("root", false, RootFn, "pos")
      val pred = AddFn(GetFn("this", false, RootFn, "(this)"), List(ConstantFn(10)), "(+)")
      val m    = MapFn(recv, pred, "(=>)")

      for (out, _) <- m.resolve(ctx)
        yield assertTrue(out == List(11,12,13))
    },

    test("Map receiver with tuple-producing predicate yields Map with new keys") {
      val data = Map("ab" -> Foo(1,"x"), "c" -> Foo(2,"y"))
      val ctx  = buildFooMapCtx("root", data)
      val recv = GetFn("root", false, RootFn, "pos")
      val newKey = LenFn(GetFn("this_key", false, RootFn, "(this_key)"), "(len)")
      val newVal = AddFn(GetFn("this_value.a", false, RootFn, "(this_value.a)"), List(ConstantFn(10)), "a")
      val pred   = Tuple2Fn(newKey.asInstanceOf[Fn[Any]], List(newVal), "(tuple)")
      val m      = MapFn(recv, pred, "(=>)")

      for (out, _) <- m.resolve(ctx)
        yield assertTrue(out == Map(2 -> 11, 1 -> 12))
    },

    test("Map receiver with non-tuple predicate yields List of computed values") {
      val data = Map("ab" -> Foo(1,"x"), "c" -> Foo(2,"y"))
      val ctx  = buildFooMapCtx("root", data)
      val recv = GetFn("root", false, RootFn, "pos")
      val pred = GetFn("this_value.b", false, RootFn, "(this_value)")
      val m    = MapFn(recv, pred, "(=>)")

      for (out, _) <- m.resolve(ctx)
        values = out.asInstanceOf[List[Any]]
      yield assertTrue(values.toSet == Set("x","y")) // order not guaranteed
    },

    test("None receiver: Tuple2 predicate → empty Map; non-tuple → empty List") {
      val dataNone: Option[List[Any]] = None
      val ctx = buildCtx("root", dataNone)
      val recvNone = GetFn("root", true, RootFn, "pos")

      val tuplePred = Tuple2Fn(ConstantFn(1), List(ConstantFn(2)), "(tuple)")
      val m1 = MapFn(recvNone, tuplePred, "(=>)")

      for (out1, _) <- m1.resolve(ctx)
      yield assertTrue(out1 == Map())
    },

    test("None receiver: non-tuple predicate → empty List") {
      val dataNone: Option[List[Any]] = None
      val ctx = buildCtx("root", dataNone)
      val recvNone = GetFn("root", true, RootFn, "pos")
      val nonTuplePred = ConstantFn(42)
      val m2 = MapFn(recvNone, nonTuplePred.asInstanceOf[Fn[Any]], "(=>)")
      for (out2, _) <- m2.resolve(ctx)
      yield assertTrue(out2 == List())
    },

    /*───────────────────────────*
     * Extra coverage for completeness
     *───────────────────────────*/

    test("distinct() on Map should fail") {
      val ctx = buildCtx("root", Map("a" -> 1))
      val fn  = DistinctFn(GetFn("root", false, RootFn, "pos"), None, "pos")
      for result <- fn.resolve(ctx).either
        yield assertTrue(result.isLeft)
    },

    test("distinct() on String should fail") {
      val ctx = buildCtx("root", "abc")
      val fn  = DistinctFn(GetFn("root", false, RootFn, "pos"), None, "pos")
      for result <- fn.resolve(ctx).either
        yield assertTrue(result.isLeft)
    },

    test("reverse() on None yields Nil") {
      val ctx = buildCtx("root", None)
      val fn  = ReverseFn(GetFn("root", true, RootFn, "pos"), "pos")
      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == Nil)
    },

    test("reverse() on non-list receiver should fail") {
      val ctx = buildCtx("root", 42)
      val fn  = ReverseFn(GetFn("root", false, RootFn, "pos"), "pos")
      for result <- fn.resolve(ctx).either
        yield assertTrue(result.isLeft)
    },

    test("limit() greater than list size just returns the list") {
      val ctx = buildCtx("root", List(1, 2, 3))
      val fn  = LimitFn(GetFn("root", false, RootFn, "pos"), 10, "pos")
      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == List(1, 2, 3))
    },

    test("clean() on empty list returns empty list") {
      val ctx = buildCtx("root", List.empty[String])
      val fn  = CleanFn(GetFn("root", false, RootFn, "pos"), "pos")
      for (v, _) <- fn.resolve(ctx)
        yield assertTrue(v == Nil)
    },

    test("MapFn: invalid body result type should fail") {
      val ctx = buildCtx("root", List(1, 2))
      // Produces a Map instead of scalar or tuple2
      val badBody: ConstantFn[Any] = ConstantFn(Map("x" -> 1))
      val fn = MapFn(GetFn("root", false, RootFn, "pos"), badBody, "pos")
      for result <- fn.resolve(ctx).either
        yield assertTrue(result.isLeft)
    }
  ).provide(
    ZLayer.succeed(RuntimeEnv(new BiMapRegistry()))
  ) @@ ziotestkit
}