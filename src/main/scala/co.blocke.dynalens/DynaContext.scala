package co.blocke.dynalens

import zio.*

final case class DynaContext(
                              symbols: Map[String, (Any, Lens)],
                              dynaLens: DynaLens[?],
                              rootObj: Any,
                              rootLens: Lens,
                              parentOpt: Option[DynaContext] = None
                            ) {

  // ---------------- Lookup ----------------

  def resolve(sym: String): Option[(Any, Lens)] =
    symbols.get(sym)

  /** Retrieve the (value, lens) pair for a symbol. */
  def get(sym: String): Option[(Any, Lens)] =
    println(s"[CTX TRACE] lookup '$sym' in ctx (locals=${symbols.keys.mkString(", ")}, parent?=${parentOpt.nonEmpty})")
    resolve(sym)

  def getSymbol(sym: String): Option[(Any, Lens)] = get(sym)

  /** Retrieve only the value for a symbol. */
  def getValue(sym: String): Option[Any] = get(sym).map(_._1)

  /** Retrieve only the lens for a symbol. */
  def getLens(sym: String): Option[Lens] = get(sym).map(_._2)


  // ---------------- Updates ----------------

  /**
   * Create or overwrite a symbol value.
   */
  def bind(sym: String, value: Any, lens: Lens): DynaContext =
    copy(symbols = symbols + (sym -> (value, lens)), dynaLens = this.dynaLens)

  /**
   * Remove a symbol binding from the context.
   */
  def unbind(sym: String): DynaContext = copy(symbols = symbols - sym, dynaLens = this.dynaLens)


  /**
   * Update 'this' object reference for the current evaluation scope.
   */
  def setThis(obj: Any, lens: Lens): DynaContext =
    copy(symbols = symbols + ("this" -> (obj, lens)), dynaLens = this.dynaLens)

  /**
   * Update 'this_key' and 'this_value' bindings used in map/loop functions.
   */
  def setThisKeyValue(k: Any, v: Any, keyLens: Lens, valLens: Lens): DynaContext =
    copy(
      symbols =
        symbols +
          ("this_key"   -> (k, keyLens)) +
          ("this_value" -> (v, valLens)),
      dynaLens = this.dynaLens
    )

  /** Retrieve current 'this' reference */
  def getThis: Option[(Any, Lens)] =
    get("this")

  /** Retrieve top-level target object */
  def getTop: Option[(Any, Lens)] =
    Some((rootObj, rootLens))


  // ---------------- Scoped Helpers ----------------

  /** Detect whether the new lens shares ancestry with an existing lens chain to prevent cycles */
  private def sharesAncestry(newLens: Lens, ancestor: Lens): Boolean =
    var cur = Option(ancestor)
    while cur.nonEmpty do
      if cur.contains(newLens) then return true
      cur = cur.flatMap(_.parent)
    false

  private def detach(lens: Lens): Lens =
    lens.copyWithParent(None)

  private inline def child(extra: (String, (Any, Lens))*): DynaContext =
    copy(symbols = symbols ++ extra, parentOpt = Some(this))

  /**
   * Executes a scoped block with a temporary 'this' binding.
   * The previous binding is automatically restored.
   */
  def withThisScoped[R](obj: Any, thisLens: Lens)(
    body: DynaContext => ZIO[RuntimeEnv, DynaLensError, R]
  ): ZIO[RuntimeEnv, DynaLensError, R] =
    // Inherit existing 'top' via child(); never rebind 'top' here.
    val newCtx = child(
      "this" -> (obj, thisLens)
    )
    body(newCtx)


  /**
   * Executes a scoped block with temporary 'this_key' and 'this_value' bindings.
   * Used primarily in LoopFn for map and filter operations.
   */
  def withThisKeyValueScoped[R](k: Any, v: Any, keyLens: Lens, valLens: Lens)(
    body: DynaContext => ZIO[RuntimeEnv, DynaLensError, R]
  ): ZIO[RuntimeEnv, DynaLensError, R] =
    println(s"[CTX TRACE] withThisKeyValueScoped ENTER")
    println(s"[CTX TRACE] keyLens=${Option(keyLens).map(_.getClass.getSimpleName)} valLens=${Option(valLens).map(_.getClass.getSimpleName)}")
    val effKeyLens = if (keyLens == null) dynaLens.topLens else keyLens
    val effValLens = if (valLens == null) dynaLens.topLens else valLens
    println(s"[CTX TRACE] effKeyLens.parent=${effKeyLens.parent.map(_.getClass.getSimpleName)} effValLens.parent=${effValLens.parent.map(_.getClass.getSimpleName)}")
    val detachedKeyLens = detach(effKeyLens)
    val detachedValLens = detach(effValLens)
    println(s"[CTX TRACE] detachedKeyLens.parent=${detachedKeyLens.parent.map(_.getClass.getSimpleName)} detachedValLens.parent=${detachedValLens.parent.map(_.getClass.getSimpleName)}")
    println(s"[CTX TRACE] parentOpt=${parentOpt.map(_ => "exists").getOrElse("none")}")

    // Anchor `this` at the owning ClassLens (not the element lens nor the ListLens)
    val ownerAnchor = {
      val base = if (valLens == null) dynaLens.topLens else valLens
      def climbToClass(l: Lens): Lens =
        l match
          case cl: ClassLens => cl
          case other => other.parent.map(climbToClass).getOrElse(dynaLens.topLens)
      climbToClass(base)
    }
    val safeAnchorForThis = detach(ownerAnchor)
    // Use the existing top object value when available, otherwise fall back to element value
    val topVal: Any = this.getTop.map(_._1).getOrElse(v)

    val newCtx = child(
      "this_key"   -> (k, detachedKeyLens),
      "this_value" -> (v, detachedValLens),
      // IMPORTANT: `this` is the element value `v`, but its lens is the CONTAINER anchor
      // so symbol lookups (e.g., `nums`) start from the collection, not the scalar element.
      "this"       -> (topVal, safeAnchorForThis)
    )
    println(s"[CTX TRACE] child context created with symbols=${newCtx.symbols.keys.mkString(", ")}")
    newCtx.symbols.foreach { case (sym, (_, lens)) =>
      println(s"[CTX TRACE]   symbol=$sym lens=${lens.getClass.getSimpleName} parent=${lens.parent.map(_.getClass.getSimpleName)}")
    }
    body(newCtx)

  override def toString: String = {
    val rows = symbols.toSeq.sortBy(_._1).map { case (k, (v, l)) =>
      val lensInfo = s"[${l.getClass.getSimpleName}:${Option(l.name).getOrElse("")}]"
      f"$k%-10s -> ${Option(v).fold("null")(_.toString)} $lensInfo"
    }
    (Seq(s"ROOT -> ${rootObj.toString} [${rootLens.getClass.getSimpleName}:${rootLens.name}]") ++ rows)
      .mkString("\n")
  }
}


object DynaContext:
  def apply(target: Any, lens: DynaLens[?]): DynaContext =
    val topLens = lens.topLens.copyWithParent(None)
    DynaContext(Map.empty, lens, target, topLens)

  def empty(lens: DynaLens[?]): DynaContext =
    DynaContext(Map.empty, lens, null, lens.topLens.copyWithParent(None))
