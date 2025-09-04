/*
 * Copyright (c) 2025 Greg Zoller
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package co.blocke.dynalens
package parser

// parser/package.scala (concept sketch)
case class Receiver(
    name: String = "this",
    fields: Map[String, Any], // element schema minus __type, or Map.empty for scalar receiver
    sym: SymbolType // Scalar, List, Map, Optional*, None
)

case class ExprContext(
    typeInfo: Map[String, Any],
    sym: Map[String, SymbolType] = Map.empty, // known symbols (vals + 'this')
    scopes: List[Map[String, Any]] = Nil, // lexical/local frames (top is head)
    receiver: Option[Receiver] = None // current “this”
) {

  def resolveSymbol(name: String): Option[SymbolType] =
    sym.get(name)

  // Helpers to derive contexts immutably:
  def withVals(newSyms: (String, SymbolType)*): ExprContext =
    copy(sym = sym ++ newSyms)

  def pushScope(m: Map[String, Any]): ExprContext =
    copy(scopes = m :: scopes)

  def withReceiverFromPath(path: String): ExprContext = {
    val targetSym = Utility.getPathType(path)(using this)
    val elemSchema = Utility.elementSchemaFor(path, typeInfo) // minus __type if element, else Map.empty
    val recv = Receiver(fields = elemSchema, sym = targetSym)
    copy(receiver = Some(recv), sym = sym + ("this" -> targetSym))
  }

  def withReceiver(recv: Receiver): ExprContext =
    copy(receiver = Some(recv), sym = sym + ("this" -> recv.sym))

  /** Merge contexts across sequential statements.
    * - Keep/union schema and symbols (later wins)
    * - Do NOT leak transient scopes/receiver across statements
    */
  def merge(that: ExprContext): ExprContext =
    this.copy(
      typeInfo = this.typeInfo ++ that.typeInfo, // allow additions like "__val_x"
      sym = this.sym ++ that.sym,
      // do not carry over local scopes/receiver from sub-parsers
      scopes = this.scopes,
      receiver = this.receiver
    )

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
}
