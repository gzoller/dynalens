package co.blocke.dynalens

import zio.*


final case class DynaContext(
                              symbols: Map[String, (Any, Lens)],
                              dynaLens: DynaLens[?]
                            ) {

  // ---------------- Lookup ----------------

  /** Retrieve the (value, lens) pair for a symbol. */
  def get(sym: String): Option[(Any, Lens)] = symbols.get(sym)

  def getSymbol(sym: String): Option[(Any, Lens)] = get(sym)

  /** Retrieve only the value for a symbol. */
  def getValue(sym: String): Option[Any] = symbols.get(sym).map(_._1)

  /** Retrieve only the lens for a symbol. */
  def getLens(sym: String): Option[Lens] = symbols.get(sym).map(_._2)


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
    get("top")


  // ---------------- Scoped Helpers ----------------

  /**
   * Executes a scoped block with a temporary 'this' binding.
   * The previous binding is automatically restored.
   */
  def withThisScoped[R](obj: Any, lens: Lens)(
    body: DynaContext => ZIO[RuntimeEnv, DynaLensError, R]
  ): ZIO[RuntimeEnv, DynaLensError, R] =
    val previous = get("this")
    val newCtx   = setThis(obj, lens)
    body(newCtx).ensuring {
      ZIO.succeed(previous match
        case Some((v, l)) => copy(symbols = symbols + ("this" -> (v, l)), dynaLens = this.dynaLens)
        case None         => copy(symbols = symbols - "this", dynaLens = this.dynaLens)
      )
    }


  /**
   * Executes a scoped block with temporary 'this_key' and 'this_value' bindings.
   * Used primarily in LoopFn for map and filter operations.
   */
  def withThisKeyValueScoped[R](k: Any, v: Any, keyLens: Lens, valLens: Lens)(
    body: DynaContext => ZIO[RuntimeEnv, DynaLensError, R]
  ): ZIO[RuntimeEnv, DynaLensError, R] =
    val prevKey = get("this_key")
    val prevVal = get("this_value")
    val prevThis = get("this")

    // new context includes all 3: this_key, this_value, and this
    val newCtx = setThisKeyValue(k, v, keyLens, valLens)
      .setThis(v, valLens)

    body(newCtx).ensuring {
      ZIO.succeed(
        copy(symbols =
          symbols
            ++ prevKey.map("this_key"   -> _)
            ++ prevVal.map("this_value" -> _)
            ++ prevThis.map("this"      -> _),
          dynaLens = this.dynaLens
        )
      )
    }
}


object DynaContext:
  def apply(target: Any, lens: DynaLens[?]): DynaContext =
    DynaContext(
      symbols = Map(
        "top" -> (target, lens.topLens),
        "this" -> (target, lens.topLens)
      ),
      dynaLens = lens
    )

  def empty(lens: DynaLens[?]): DynaContext =
    DynaContext(Map.empty, lens)



object CtxStrings:
  /** Pretty-print ctx. Ephemeral keys (`this`, `name[]`) are hidden by default. */
  def toStringCtx(ctx: DynaContext, includeEphemeral: Boolean = false): String = {
    inline def isEphemeral(k: String): Boolean = k == "this" || k == "this_key" || k == "this_value"

    // (key, valueOnly) sequence, filtered
    val base: Seq[(String, Any)] =
      ctx.symbols.iterator
        .filterNot { case (k, _) => !includeEphemeral && isEphemeral(k) }
        .map { case (k, (v, _)) => (k, v) }
        .toSeq

    // order: "top" first, then alpha
    val ordered =
      base.sortBy { case (k, _) => if k == "top" then "\u0000" else k }

    // match your test snapshots: "key -> value.toString"
    ordered
      .map { case (k, v) => s"$k -> ${Option(v).fold("null")(_.toString)}" }
      .mkString("", "\n", "\n")
  }
