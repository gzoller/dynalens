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

import zio.*

trait Statement:
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, DynaContext]

case class ValStmt[R](name: String, fn: Fn[R]) extends Statement:
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, DynaContext] =
    for {
      (value, lens) <- fn.resolve(ctx)
    } yield ctx.bind(name, value, lens)

case class MapStmt(path: String, fn: Fn[Any], posStr: String) extends Statement:
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, DynaContext] =
    ctx.get("top") match
      case Some((root, topLens)) =>
        /*
        topLens match
          case Some(lens) =>
            // for
            //   mapped <- MapRuntime.mapOver(
            //     path = path,
            //     predicate = fn,
            //     root = root,
            //     lens = lens.asInstanceOf[DynaLens[Any]],
            //     posStr = posStr,
            //     outerCtx = ctx
            //   )
            // yield ctx.clone.addOne("top", (mapped, topLens))
            ZIO.fail(DynaLensError(posStr, "MapStmt not yet implemented (TODO)"))
          case None =>
            ZIO.fail(DynaLensError(posStr, "MapStmt requires top lens in context"))
         */
        ZIO.fail(DynaLensError(posStr, "Boom!  TODO--temporary error"))
      case None =>
        ZIO.fail(DynaLensError(posStr, "Missing 'top' in context for map statement"))


case class IfStmt(
    condition: Fn[Boolean],
    thenBlock: Statement,
    elseBlock: Option[Statement] = None
) extends Statement {

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, DynaContext] =
    for {
      (condAny, _) <- condition.resolve(ctx)
      cond = condAny.asInstanceOf[Boolean]
      resultCtx <-
        if cond then thenBlock.resolve(ctx)
        else elseBlock.map(_.resolve(ctx)).getOrElse(ZIO.succeed(ctx))
    } yield resultCtx
}


case class BlockStmt(statements: Seq[Statement]) extends Statement:
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, DynaContext] =
    statements.foldLeft[ZIO[RuntimeEnv, DynaLensError, DynaContext]](ZIO.succeed(ctx)) { (accZio, stmt) =>
      accZio.flatMap { accCtx =>
        stmt.resolve(accCtx)
      }
    }


case class UpdateStmt[R](path: String, valueFn: Fn[R], posStr: String) extends Statement:

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, DynaContext] =
    ctx.get("top") match
      case None =>
        ZIO.fail(DynaLensError(posStr, "Missing 'top' in context for update statement"))

      case Some((rootObj, rootLens)) =>
        if rootObj == null then
          ZIO.fail(DynaLensError(posStr, "Receiver object is null — cannot update"))
        else
          val elements0 = Path.parsePath(path)
          val startsWithThis = elements0.headOption match
            case Some(PathElement(Some("this"), None)) => true
            case _                                      => false
          val elements = if startsWithThis then elements0.tail else elements0

          for
            ctxWithThis   <- ZIO.succeed(ctx.bind("this", rootObj, rootLens))
            (newValue, _) <- valueFn.resolve(ctxWithThis)

            updatedObj <-
              if startsWithThis && (rootObj == None) then
                // Optional parent missing and path anchored to `this` → no-op, preserve original root object
                ZIO.succeed(rootObj)
              else
                ZIO
                  .attempt(rootLens.update(elements, newValue, rootObj))
                  .flatten
                  .mapError {
                    case _: ClassCastException =>
                      DynaLensError(
                        posStr,
                        s"Type mismatch: cannot assign value of type ${newValue.getClass.getName} to path '$path'"
                      )
                    case e =>
                      DynaLensError(posStr, s"Unexpected update error: ${e.getMessage}")
                  }

            updatedCtx = ctx.bind("top", updatedObj, rootLens)
          yield updatedCtx.unbind("this")