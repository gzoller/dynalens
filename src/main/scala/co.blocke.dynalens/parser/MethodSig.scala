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

object MethodSig:

  def lookup(name: String): Option[MethodSig] = methodSigs.get(name)

  trait MethodSig:
    /** Accepted receiver kinds */
    def accepts(receiver: FieldType): Boolean

    /** Result kind given the receiver kind */
    def result(receiver: FieldType): FieldType

  private object StartsWithSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean = receiver match
      case ScalarType(_, "java.lang.String") => true
      case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType(name = "<anon>", typeName = "scala.Boolean")

  private object EndsWithSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ScalarType(_, "java.lang.String") => true
        case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType("", "scala.Boolean")

  private object ContainsSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean = receiver match
      // String.contains(...)
      case ScalarType(_, "java.lang.String") => true
      // Collections: List or Map (including Option[List] / Option[Map])
      case ListType(_, _, _) => true
      case MapType(_, _, _, _) => true
      case OptionType(_, inner, _) => accepts(inner) // unwrap Option and retry
      case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType("", "scala.Boolean")

  private object EqualsIgnoreCaseSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ScalarType(_, "java.lang.String") => true
        case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType("", "scala.Boolean")

  // String.matches(<regex>)
  private object MatchesRegexSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean = receiver match
      case ScalarType(_, "java.lang.String") => true
      case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType("", "scala.Boolean")

  // OptionalValue.else(<default>)
  private object ElseSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean = receiver match
      case OptionType(_, valueType, _) => valueType.isInstanceOf[ScalarType]
      // conceptually covers "None" as an empty Option
      case _ => false
    def result(receiver: FieldType): FieldType = receiver match
      // unwrap and return the underlying scalar type
      case OptionType(_, valueType: ScalarType, _) => valueType
      case _ => ScalarType("", "scala.Any")

  private object IsDefinedSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean = receiver match
      case _: OptionType => true
      // historically `None` (missing value) also passed; treat empty Option the same
      case ScalarType(_, "scala.None.type") => true
      case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType("", "scala.Boolean")

  private object LenSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean = receiver match
      case _: ListType => true
      case _: MapType => true
      case OptionType(_, v, _) if v.isInstanceOf[ListType] || v.isInstanceOf[MapType] => true
      case ScalarType(_, t) if t == "java.lang.String" => true
      case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType("", "scala.Int")

  private object ToUpperCaseSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ScalarType(_, typeName) if typeName == "java.lang.String" => true
        case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType(receiver.name, "java.lang.String")

  private object ToLowerCaseSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ScalarType(_, typeName) if typeName == "java.lang.String" => true
        case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType(receiver.name, "java.lang.String")

  private object TrimSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ScalarType(_, typeName) if typeName == "java.lang.String" => true
        case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType(receiver.name, "java.lang.String")

  private object TemplateSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ScalarType(_, typeName) if typeName == "java.lang.String" => true
        case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType(receiver.name, "java.lang.String")

  private object SubstrSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ScalarType(_, typeName) if typeName == "java.lang.String" => true
        case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType(receiver.name, "java.lang.String")

  private object ReplaceSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ScalarType(_, typeName) if typeName == "java.lang.String" => true
        case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType(receiver.name, "java.lang.String")

  private object DateFmtSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ScalarType(_, typeName) if typeName == "java.util.Date" => true
        case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType(receiver.name, "java.lang.String")

  private object ToDateSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ScalarType(_, typeName) if typeName == "java.lang.String" => true
        case _ => false
    def result(receiver: FieldType): FieldType =
      ScalarType(receiver.name, "java.util.Date")

  private object NowSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      true // or: receiver.isInstanceOf[ScalarType] etc.
    def result(receiver: FieldType): FieldType =
      ScalarType(receiver.name, "java.util.Date")

  private object UuidSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      true // or: receiver.isInstanceOf[ScalarType] etc.
    def result(receiver: FieldType): FieldType =
      ScalarType(receiver.name, "java.util.UUID")

  private object SortAscSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ListType(_, _, _) => true
        case OptionType(_, ListType(_, _, _), "scala.Option") => true
        case _ => false
    def result(receiver: FieldType): FieldType = receiver

  private object SortDescSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ListType(_, _, _) => true
        case OptionType(_, ListType(_, _, _), "scala.Option") => true
        case _ => false
    def result(receiver: FieldType): FieldType = receiver

  private object FilterSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ListType(_, _, _) => true
        case OptionType(_, ListType(_, _, _), "scala.Option") => true
        case _ => false
    def result(receiver: FieldType): FieldType = receiver

  private object DistinctSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ListType(_, _, _) => true
        case OptionType(_, ListType(_, _, _), "scala.Option") => true
        case _ => false
    def result(receiver: FieldType): FieldType = receiver

  private object LimitSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ListType(_, _, _) => true
        case OptionType(_, ListType(_, _, _), "scala.Option") => true
        case _ => false
    def result(receiver: FieldType): FieldType = receiver

  private object ReverseAscSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ListType(_, _, _) => true
        case OptionType(_, ListType(_, _, _), "scala.Option") => true
        case _ => false
    def result(receiver: FieldType): FieldType = receiver

  private object CleanSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean =
      receiver match
        case ListType(_, _, _) => true
        case OptionType(_, ListType(_, _, _), "scala.Option") => true
        case _ => false
    def result(receiver: FieldType): FieldType = receiver

  private object KeysSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean = receiver match
      case MapType(_, keyT: ScalarType, _, tn) if tn.startsWith("scala.collection.immutable.Map") => true
      case OptionType(_, MapType(_, keyT: ScalarType, _, tn), "scala.Option") if tn.startsWith("scala.collection.immutable.Map") => true
      case _ => false
    def result(receiver: FieldType): FieldType = receiver match
      case m: MapType =>
        ListType("", m.keyType, s"scala.collection.immutable.List[${m.keyType.typeName}]")
      case o: OptionType if o.valueType.isInstanceOf[MapType] =>
        val m = o.valueType.asInstanceOf[MapType]
        OptionType("", ListType("", m.keyType, s"scala.collection.immutable.List[${m.keyType.typeName}]"), "scala.Option")
      case _ =>
        ListType("", ScalarType("", "scala.Any"), "scala.collection.immutable.List[scala.Any]")


  private object ValuesSig extends MethodSig:
    def accepts(receiver: FieldType): Boolean = receiver match
      case MapType(_, keyT: ScalarType, _, tn) if tn.startsWith("scala.collection.immutable.Map") => true
      case OptionType(_, MapType(_, keyT: ScalarType, _, tn), "scala.Option") if tn.startsWith("scala.collection.immutable.Map") => true
      case _ => false
    def result(receiver: FieldType): FieldType = receiver match
      case m: MapType =>
        ListType("", m.valueType, s"scala.collection.immutable.List[${m.valueType.typeName}]")
      case o: OptionType if o.valueType.isInstanceOf[MapType] =>
        val m = o.valueType.asInstanceOf[MapType]
        OptionType("", ListType("", m.valueType, s"scala.collection.immutable.List[${m.valueType.typeName}]"), "scala.Option")
      case _ =>
        ListType("", ScalarType("", "scala.Any"), "scala.collection.immutable.List[scala.Any]")

  private val methodSigs: Map[String, MethodSig] = Map(
    // scalar/string ops
    M_STARTSWITH -> StartsWithSig,
    M_ENDSWITH -> EndsWithSig,
    M_CONTAINS -> ContainsSig,
    M_EQUALSIGNORECASE -> EqualsIgnoreCaseSig,
    M_MATCHESREGEX -> MatchesRegexSig,
    M_TOUPPERCASE -> ToUpperCaseSig,
    M_TOLOWERCASE -> ToLowerCaseSig,
    M_TRIM -> TrimSig,
    M_TEMPLATE -> TemplateSig,
    M_SUBSTR -> SubstrSig,
    M_REPLACE -> ReplaceSig,
    M_DATEFMT -> DateFmtSig,
    M_TODATE -> ToDateSig,
    "now" -> NowSig, // no M_NOW provided
    "uuid" -> UuidSig, // no M_UUID provided

    // option/none helpers
    M_ELSE -> ElseSig,
    M_ISDEFINED -> IsDefinedSig,

    // generic length
    M_LEN -> LenSig,

    // map methods
    // M_GET -> GetSig, // (commented out in your original)
    M_KEYS -> KeysSig,
    M_VALUES -> ValuesSig,

    // collection (Seq) methods
    M_SORTASC -> SortAscSig,
    M_SORTDESC -> SortDescSig,
    M_FILTER -> FilterSig,
    M_DISTINCT -> DistinctSig,
    M_LIMIT -> LimitSig,
    M_REVERSE -> ReverseAscSig,
    M_CLEAN -> CleanSig
  )
