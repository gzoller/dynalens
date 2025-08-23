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

// somewhere shared (e.g., near DynaContext)
def withElemCtx(elem: Any, ctx: DynaContext): DynaContext =
  // store the element; no lens associated
  ctx.updatedWith("this", (elem, None))

import zio.*

def withLoopSymbol[R](
                               ctx: DynaContext,
                               key: String,
                               value: Any,
                               lens: Option[DynaLens[?]]
                             )(body: ZIO[_BiMapRegistry, DynaLensError, R]): ZIO[_BiMapRegistry, DynaLensError, R] =
  ZIO.acquireReleaseWith(
    ZIO.succeed {
      val prev = ctx.get(key) // remember what was there (if anything)
      ctx.update(key, (value, lens)) // shadow / insert
      prev
    }
  )(prevOpt =>
    ZIO.succeed {
      prevOpt match {
        case Some(prev) => ctx.update(key, prev) // restore prior binding
        case None => ctx.remove(key) // remove if we introduced it
      }
      ()
    }
  )(_ => body)
   
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


def asSeq(v: Any, where: String): Either[DynaLensError, Seq[Any]] =
  v match {
    case null                  => Right(Seq.empty)               // be lenient
    case s: Seq[_]             => Right(s.asInstanceOf[Seq[Any]])
    case it: Iterable[_]       => Right(it.toSeq.asInstanceOf[Seq[Any]])
    case other                 => Left(DynaLensError(s"$where expected a collection, got: ${other.getClass.getSimpleName}"))
  }
