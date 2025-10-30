package co.blocke.dynalens

import scala.quoted.*


final case class DynaLensError(posStr: String, msg: String) extends Exception(s"(runtime) $posStr Error: $msg")

enum MapKeyKind:
  case StringKey
  case IntKey
  case LongKey
  case EnumKey(enumClassName: String)


given ToExpr[MapKeyKind] with
  def apply(mkk: MapKeyKind)(using Quotes): Expr[MapKeyKind] = mkk match
    case MapKeyKind.StringKey => '{ MapKeyKind.StringKey }
    case MapKeyKind.IntKey    => '{ MapKeyKind.IntKey }
    case MapKeyKind.LongKey   => '{ MapKeyKind.LongKey }
    case MapKeyKind.EnumKey(enumClassName) =>
      '{ MapKeyKind.EnumKey(${ Expr(enumClassName) }) }

extension (s: String)
  def decapitalize: String =
    if s.isEmpty then s
    else s"${s.head.toLower}${s.tail}"

given ToExpr[ClassType] with
  def apply(ct: ClassType)(using Quotes): Expr[ClassType] =
    '{ ClassType(${ Expr(ct.name) }, ${ Expr(ct.typeName) }, ${ Expr(ct.fields) }, ${ Expr(ct.isOptional) }) }

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

    case EnumType(fieldName, validValues, typeName, isOptional) =>
      '{ EnumType(${ Expr(fieldName) }, ${ Expr(validValues) }, ${ Expr(typeName) }, ${ Expr(isOptional) }) }