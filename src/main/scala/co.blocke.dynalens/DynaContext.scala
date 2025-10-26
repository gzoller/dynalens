package co.blocke.dynalens

import zio.*


final case class DynaContext(
                              symbols: Map[String, (Any, Lens)]
                            ) {

  // ---------------- Lookup ----------------

  /** Retrieve the (value, lens) pair for a symbol. */
  def get(sym: String): Option[(Any, Lens)] = symbols.get(sym)

  /** Retrieve only the value for a symbol. */
  def getValue(sym: String): Option[Any] = symbols.get(sym).map(_._1)

  /** Retrieve only the lens for a symbol. */
  def getLens(sym: String): Option[Lens] = symbols.get(sym).map(_._2)


  // ---------------- Updates ----------------

  /**
   * Create or overwrite a symbol value.
   */
  def bind(sym: String, value: Any, lens: Lens): DynaContext =
    copy(symbols = symbols + (sym -> (value, lens)))

  /**
   * Remove a symbol binding from the context.
   */
  def unbind(sym: String): DynaContext = copy(symbols = symbols - sym)


  /**
   * Update 'this' object reference for the current evaluation scope.
   */
  def setThis(obj: Any, lens: Lens): DynaContext =
    copy(symbols = symbols + ("this" -> (obj, lens)))

  /**
   * Update 'this.key' and 'this.value' bindings used in map/loop functions.
   */
  def setThisKeyValue(k: Any, v: Any, keyLens: Lens, valLens: Lens): DynaContext =
    copy(
      symbols =
        symbols +
          ("this.key"   -> (k, keyLens)) +
          ("this.value" -> (v, valLens))
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
        case Some((v, l)) => copy(symbols = symbols + ("this" -> (v, l)))
        case None         => copy(symbols = symbols - "this")
      )
    }


  /**
   * Executes a scoped block with temporary 'this.key' and 'this.value' bindings.
   * Used primarily in LoopFn for map and filter operations.
   */
  def withThisKeyValueScoped[R](k: Any, v: Any, keyLens: Lens, valLens: Lens)(
    body: DynaContext => ZIO[RuntimeEnv, DynaLensError, R]
  ): ZIO[RuntimeEnv, DynaLensError, R] =
    val prevKey = get("this.key")
    val prevVal = get("this.value")
    val newCtx  = setThisKeyValue(k, v, keyLens, valLens)
    body(newCtx).ensuring {
      ZIO.succeed(
        copy(symbols =
          symbols
            ++ prevKey.map("this.key"   -> _)
            ++ prevVal.map("this.value" -> _)
        )
      )
    }
}
