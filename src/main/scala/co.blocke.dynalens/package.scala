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

import scala.quoted.*

final case class DynaLensError(posStr: String, msg: String) extends Exception(s"(runtime) $posStr Error: $msg")

final case class DynaLensBuildContext(quotes: Quotes)

extension (s: String)
  def decapitalize: String =
    if s.isEmpty then s
    else s"${s.head.toLower}${s.tail}"


// Tiny record exposed to scripts when mapping over Map[K, V]
final case class EntryKV(key: Any, value: Any)

import scala.quoted.*

given ToExpr[ClassType] with
  def apply(ct: ClassType)(using Quotes): Expr[ClassType] =
    '{ ClassType(${ Expr(ct.fieldName) }, ${ Expr(ct.typeName) }, ${ Expr(ct.fields) }, ${ Expr(ct.isOptional) }) }

given ToExpr[FieldType] with
  def apply(ft: FieldType)(using Quotes): Expr[FieldType] = ft match
    case ScalarType(fieldName, typeName, isOptional) =>
      '{ ScalarType(${ Expr(fieldName) }, ${ Expr(typeName) }, ${ Expr(isOptional) }) }

    case ListType(fieldName, elementType, typeName, isOptional) =>
      '{ ListType(${ Expr(fieldName) }, ${ Expr(elementType) }, ${ Expr(typeName) }, ${ Expr(isOptional) }) }

    case MapType(fieldName, keyType, valueType, typeName, isOptional) =>
      '{ MapType(${ Expr(fieldName) }, ${ Expr(keyType) }, ${ Expr(valueType) }, ${ Expr(typeName) }, ${ Expr(isOptional) }) }

    case ClassType(fieldName, typeName, fields, isOptional) =>
      '{ ClassType(${ Expr(fieldName) }, ${ Expr(typeName) }, ${ Expr(fields) }, ${ Expr(isOptional) }) }

    case ValType(fieldName, valueType, typeName) =>
      '{ ValType(${ Expr(fieldName) }, ${ Expr(valueType) }, ${ Expr(typeName) }) }


//============= JUNK PLACEHOLDERS ==========
import zio.*
import fastparse.*
case class DynaLens[T](_schema: ClassType, _registry: Map[String, DynaLens[?]]):
  type ThisT = T
  def get(path: String, obj: T): ZIO[Any, DynaLensError, Any] =
    ZIO.succeed("boom")
  private[dynalens] def walkPath(
        path: List[Path.PathElement],
        current: Any
      ): ZIO[Any, DynaLensError, DynaContext] = ZIO.succeed(DynaContext.empty)
  private[dynalens] def getValue(
                                  pathElements: List[Path.PathElement],
                                  obj: T
                                ): ZIO[Any, DynaLensError, Any] = ZIO.succeed("yay")
  def update(path: String, value: Any, obj: T): ZIO[_BiMapRegistry, DynaLensError, T] = ZIO.succeed(obj)
  private[dynalens] def updateValue(pathElements: List[Path.PathElement], value: Any, obj: T): ZIO[_BiMapRegistry, DynaLensError, T] = ZIO.succeed(obj)
  private[dynalens] def lensForPathPrefix(root: DynaLens[?], parts: List[Path.PathElement]): Option[DynaLens[?]] = None


case class BlockStmt()

trait Statement():
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, DynaContext] = ZIO.fail(DynaLensError("boom","ugh"))

object Grammar:
  def topLevelBlock[$: P](using exprCtx: parser.ExprContext): P[Either[parser.DLCompileError, BlockStmt]] =
    Fail.opaque("Boom")

case class ValStmt[R](name: String, fn: Fn[R]) extends Statement