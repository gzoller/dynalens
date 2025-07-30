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
import Path.*

trait Statement:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, DynaContext]

case class ValStmt[R](name: String, fn: Fn[R]) extends Statement:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, DynaContext] =
    for {
      value <- fn.resolve(ctx)
    } yield ctx.updatedWith(name, (value, null))

case class MapStmt(path: String, fn: Fn[?]) extends Statement:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, DynaContext] =
    ctx.get("top") match {
      case Some((root, topLens)) =>
        for {
          mapped <- topLens.map(path, fn, root.asInstanceOf[topLens.ThisT], outerCtx = ctx) // <-- pass ctx here
        } yield ctx.clone.addOne("top", (mapped, topLens))
      case None =>
        ZIO.fail(DynaLensError("Missing 'top' in context for map operation"))
    }

case class IfStmt(
    condition: Fn[Boolean],
    thenBlock: Statement,
    elseBlock: Option[Statement] = None
) extends Statement {

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, DynaContext] =
    for {
      cond <- condition.resolve(ctx)
      resultCtx <-
        if cond then thenBlock.resolve(ctx)
        else elseBlock.map(_.resolve(ctx)).getOrElse(ZIO.succeed(ctx))
    } yield resultCtx
}

case class BlockStmt(statements: Seq[Statement]) extends Statement:
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, DynaContext] =
    statements.foldLeft(ZIO.succeed(ctx): ZIO[_BiMapRegistry, DynaLensError, DynaContext]) { (accZio, stmt) =>
      accZio.flatMap { accCtx =>
        stmt.resolve(accCtx)
      }
    }

case class UpdateStmt[R](
    path: String,
    valueFn: Fn[R]
) extends Statement:

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, DynaContext] =
    parsePath(path) match
      case Nil =>
        ZIO.fail(DynaLensError("update requires a path"))

      case pathHead :: rest if ctx.contains(pathHead.name) =>
        ctx(pathHead.name) match
          case (obj, null) =>
            ZIO.fail(DynaLensError(s"Cannot update val '${pathHead.name}' (no lens)"))
          case (obj, dyanlens) =>
            for {
              value <- valueFn.resolve(ctx)
              updatedObj <- dyanlens
                .asInstanceOf[DynaLens[Any]]
                .update(partialPath(rest), value, obj.asInstanceOf[dyanlens.ThisT])
            } yield ctx.clone().addOne(pathHead.name, (updatedObj, dyanlens))

      case _ =>
        ctx.get("top") match
          case Some((obj, dynalens)) =>
            for {
              value <- valueFn.resolve(ctx)
              updatedObj <- dynalens
                .asInstanceOf[DynaLens[Any]]
                .update(path, value, obj.asInstanceOf[dynalens.ThisT])
            } yield ctx.clone().addOne("top", (updatedObj, dynalens))
          case None =>
            ZIO.fail(DynaLensError(s"Unable to update: no 'top' context found for path $path"))
