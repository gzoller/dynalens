package co.blocke.dynalens
package parser

// parser/package.scala (concept sketch)
case class Receiver(
                     name: String = "this",
                     fields: Map[String, Any],          // element schema minus __type, or Map.empty for scalar receiver
                     sym: SymbolType                    // Scalar, List, Map, Optional*, None
                   )

case class ExprContext(
                        typeInfo: Map[String, Any],
                        sym: Map[String, SymbolType] = Map.empty,       // known symbols (vals + 'this')
                        scopes: List[Map[String, Any]] = Nil,           // lexical/local frames (top is head)
                        receiver: Option[Receiver] = None               // current “this”
                      ) {
  // Shadowing: scopes > receiver.fields > typeInfo
  def resolveField(name: String): Option[Any] =
    scopes.collectFirst { case m if m.contains(name) => m(name) }
      .orElse(receiver.flatMap(r => r.fields.get(name)))
      .orElse(typeInfo.get(name))

  def resolveSymbol(name: String): Option[SymbolType] =
    sym.get(name)

  // Helpers to derive contexts immutably:
  def withVals(newSyms: (String, SymbolType)*): ExprContext =
    copy(sym = sym ++ newSyms)

  def pushScope(m: Map[String, Any]): ExprContext =
    copy(scopes = m :: scopes)

  def withReceiverFromPath(path: String): ExprContext = {
    val targetSym   = Utility.getPathType(path)
    val elemSchema  = Utility.elementSchemaFor(path, typeInfo) // minus __type if element, else Map.empty
    val recv        = Receiver(fields = elemSchema, sym = targetSym)
    copy(receiver = Some(recv), sym = sym + ("this" -> targetSym))
  }

  override def toString: String = {
    def fmtMap(m: Map[?, ?], indent: String = "  "): String =
      if m.isEmpty then "{}"
      else {
        val body = m.iterator
          .map { case (k, v) => s"$indent$k -> $v" }
          .mkString("\n")
        s"{\n$body\n}"
      }

    def fmtScopes(sc: List[Map[String, Any]]): String =
      if sc.isEmpty then "[]"
      else {
        val body = sc.zipWithIndex
          .map { case (m, i) => s"  scope[$i] = ${fmtMap(m, "    ")}" }
          .mkString("\n")
        s"[\n$body\n]"
      }

    val recvStr = receiver.map(_.toString).getOrElse("None")

    s"""ExprContext(
       |  typeInfo  = ${fmtMap(typeInfo)}
       |  sym       = ${fmtMap(sym)}
       |  scopes    = ${fmtScopes(scopes)}
       |  receiver  = $recvStr
       |)\n----------------------""".stripMargin
  }
//  def clearReceiver: ExprContext = copy(receiver = None, sym = sym - "this")
}