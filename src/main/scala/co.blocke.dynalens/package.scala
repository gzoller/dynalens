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

//
// DynaContext used during runtime execution of compiled scripts
//
// Map[ symbol, (value, lens?) ]
type DynaContext = scala.collection.mutable.Map[String, (Any, Option[DynaLens[?]])]

object DynaContext:
  def apply(target: Any, lens: Option[DynaLens[?]]): DynaContext =
    scala.collection.mutable.Map("top" -> (target, lens))

  def empty: DynaContext = scala.collection.mutable.Map.empty

extension (ctx: DynaContext)
  def updatedWith(k: String, v: (Any, Option[DynaLens[?]])): DynaContext =
    val copy = ctx.clone().asInstanceOf[DynaContext]
    copy += (k -> v)
    copy


final case class DynaLensError (msg: String) extends Exception(msg)
//final case class CompileException (rendered: String) extends Exception (rendered)

//
// ExprContext used during compilation
//
enum SymbolType:
  case Exempt // eg top
  case Normal
  case OptionalScalar
  case OptionalScalaWithDefault
  case OptionalList
  case OptionalMap

case class ExprContext(sym: Map[String, SymbolType] = Map.empty, searchThis: Boolean = false)

given defaultExprContext: ExprContext = ExprContext()

def unwrapOption(
    path: String,
    obj: Any,
    elseValue: Option[Fn[Any]],
    isDefined: Boolean,
    useRawValue: Boolean = false // don't unwrap Option
): Either[DynaLensError, Either[Fn[Any], Any]] =
  obj match
    case opt: Option[?] =>
      if isDefined then Right(Right(opt.isDefined))
      else if path.endsWith("[]") then Right(Right(opt.getOrElse(Nil)))
      else if useRawValue then Right(Right(opt))
      else if elseValue.isDefined then
        opt match
          case Some(actual) => Right(Right(actual))
          case None =>
            Right(Left(elseValue.get)) // defer resolution
      else Left(DynaLensError(s"Access to optional field '$path' requires .else(...)"))

    case other =>
      Right(Right(other))
