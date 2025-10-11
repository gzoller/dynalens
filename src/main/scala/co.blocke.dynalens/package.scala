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
    '{ ClassType(${ Expr(ct.fieldName) }, ${ Expr(ct.typeName) }, ${ Expr(ct.fields) }) }

given ToExpr[SealedTraitType] with
  def apply(st: SealedTraitType)(using Quotes): Expr[SealedTraitType] =
    '{ SealedTraitType(${ Expr(st.fieldName) }, ${ Expr(st.typeName) }, ${ Expr(st.fields) }, ${ Expr(st.subTypes) }) }

given ToExpr[FieldType] with
  def apply(ft: FieldType)(using Quotes): Expr[FieldType] = ft match
    case ScalarType(fieldName, typeName) =>
      '{ ScalarType(${ Expr(fieldName) }, ${ Expr(typeName) }) }

    case OptionType(fieldName, valueType, typeName) =>
      '{ OptionType(${ Expr(fieldName) }, ${ Expr(valueType) }, ${ Expr(typeName) }) }

    case ListType(fieldName, elementType, typeName, isOptional) =>
      '{ ListType(${ Expr(fieldName) }, ${ Expr(elementType) }, ${ Expr(typeName) }, ${ Expr(isOptional) }) }

    case MapType(fieldName, keyType, valueType, typeName, isOptional) =>
      '{ MapType(${ Expr(fieldName) }, ${ Expr(keyType) }, ${ Expr(valueType) }, ${ Expr(typeName) }, ${ Expr(isOptional) }) }

    case ct: ClassType =>
      Expr(ct)

    case st: SealedTraitType =>
      Expr(st)




//============= JUNK PLACEHOLDERS ==========
import zio.*
import fastparse.*
case class DynaLens[T](_schema: ClassType):
  type ThisT = T
  def get(path: String, obj: T): ZIO[Any, DynaLensError, Any] =
    ZIO.succeed("boom")

case class BlockStmt()

case class Statement()

object Grammar:
  def topLevelBlock[$: P](using exprCtx: parser.ExprContext): P[Either[parser.DLCompileError, BlockStmt]] =
    Fail.opaque("Boom")
