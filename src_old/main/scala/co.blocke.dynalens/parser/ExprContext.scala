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

import co.blocke.dynalens.fn.GetFn

case class ExprContext(
                        scriptText: String,
                        schema: ClassType,
                        symbols: List[Map[String, FieldType]] = Nil,
                        receiver: Option[Receiver] = None,
                        pos: Int = 0
                      ) {

  def pushScope(fields: List[FieldType]): ExprContext =
    copy(symbols = fields.map(ft => ft.name -> ft).toMap :: symbols)

  def resolveSymbol(name: String): Option[FieldType] =
    symbols.collectFirst { case m if m.contains(name) => m(name) }

  /** Add value bindings immutably. */
  def withVals(newVals: (String, FieldType)*): ExprContext =
    val updatedHead = symbols.headOption.getOrElse(Map.empty) ++ newVals
    copy(symbols = updatedHead :: symbols.drop(1))

  // For error messages
  val posStr: String = posStrFrom(pos)
  def posStrFrom(pos: Int): String =
    val lines = scriptText.take(pos).split('\n')
    val line  = lines.length
    val col   = lines.lastOption.map(_.length).getOrElse(0) + 1
    s"[$line,$col]"

  /** Install a new receiver from a given schema path. */
  def withReceiverFromPath(path: String): Either[DLCompileError, ExprContext] =
    Utility.getPathType(path)(using this).map { targetField =>
      val fn = GetFn(path, targetField.isOptional, co.blocke.dynalens.fn.RootFn, posStr)
      this.copy(receiver = Some(NamedReceiver(path, targetField, fn)))
    }

  /** Directly set a receiver object. */
  def withReceiver(recv: Receiver): ExprContext =
    copy(receiver = Some(recv))

  /** Merge contexts across sequential statements without leaking receiver. */
  def merge(that: ExprContext): ExprContext =
    ExprContext(
      scriptText = this.scriptText,
      schema     = this.schema,
      symbols    = that.symbols ++ this.symbols,
      receiver   = this.receiver,
      pos        = this.pos
    )

  override def toString: String =
    val recvStr   = receiver.map(_.toString).getOrElse("None")
    val schemaStr = Option(schema).map(_.toString).getOrElse("None")
    val symsStr   = if symbols.isEmpty then "{}"
    else symbols.map(m => m.map { case (k,v) => s"$k -> $v" }.mkString("{", ", ", "}")).mkString("\n")
    s"""ExprContext(
       |  schema   = $schemaStr
       |  symbols  = $symsStr
       |  receiver = $recvStr
       |  pos      = $pos
       |)""".stripMargin
}