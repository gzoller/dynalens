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

final case class DynaLensError(msg: String) extends Exception(msg)

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
    case null            => Right(Seq.empty) // be lenient
    case s: Seq[?]       => Right(s.asInstanceOf[Seq[Any]])
    case it: Iterable[?] => Right(it.toSeq.asInstanceOf[Seq[Any]])
    case other           => Left(DynaLensError(s"$where expected a collection, got: ${other.getClass.getSimpleName}"))
  }

// Tiny record exposed to scripts when mapping over Map[K, V]
final case class EntryKV(key: Any, value: Any)

/*
// Lens for EntryKV so GetFn("this.key") / GetFn("this.value") can descend
val entryLens: DynaLens[EntryKV] =
  DynaLens[EntryKV](
    _update = (field, v, obj) =>
      field match {
        case "key"   => ZIO.succeed(obj.copy(key = v))
        case "value" => ZIO.succeed(obj.copy(value = v))
        case other   => ZIO.fail(DynaLensError(s"EntryKV: no such field '$other'"))
      },
    _get = (field, obj) =>
      field match {
        case "key"   => ZIO.succeed(Some(obj.key))
        case "value" => ZIO.succeed(Some(obj.value))
        case other   => ZIO.fail(DynaLensError(s"EntryKV: no such field '$other'"))
      },
    _registry = Map.empty, // no nested fields
    _typeName = "EntryKV",
    _typeInfo = Map("key" -> "", "value" -> "", "__type" -> "{}"),
    _schema = Schema.buildSchema(
      co.blocke.scala_reflection.RType.of[EntryKV]
        .asInstanceOf[co.blocke.scala_reflection.impl.ScalaClassRef[?]]
    ),
    _elemIsOptional = Map.empty
  )
  */

import scala.quoted.*

given ToExpr[FieldType] with
  def apply(ft: FieldType)(using Quotes): Expr[FieldType] = ft match
    case ScalarType(name, typeName) =>
      '{ ScalarType(${Expr(name)}, ${Expr(typeName)}) }
    case OptionType(name, valueType, typeName) =>
      '{ OptionType(${Expr(name)}, ${Expr(valueType)}, ${Expr(typeName)}) }
    case ListType(name, elemType, typeName) =>
      '{ ListType(${Expr(name)}, ${Expr(elemType)}, ${Expr(typeName)}) }
    case MapType(name, keyType, valueType, typeName) =>
      '{ MapType(${Expr(name)}, ${Expr(keyType)}, ${Expr(valueType)}, ${Expr(typeName)}) }
    case ClassType(name, typeName) =>
      '{ ClassType(${Expr(name)}, ${Expr(typeName)}) }
    case ParamClassType(name, typeName, schema) =>
      '{ ParamClassType(${Expr(name)}, ${Expr(typeName)}, ${Expr(schema)}) }

given ToExpr[Schema] with
  def apply(s: Schema)(using Quotes): Expr[Schema] =
    '{ Schema(${Expr(s.className)}, ${Expr(s.fields)}, ${Expr(s.catalog)}) }