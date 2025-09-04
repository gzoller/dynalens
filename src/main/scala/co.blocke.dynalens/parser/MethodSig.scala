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

  trait MethodSig:
    /** Accepted receiver kinds (coarse). E.g., Set(List) for filter/sort. */
    def upon: Set[SymbolType]

    /** Result kind given the receiver kind (coarse). */
    def out(receiver: SymbolType): SymbolType

  /** (Optional) validate args against receiver kind; return message when invalid. */
  //    def validateArgs(receiver: SymbolType, args: List[Fn[?]]): Option[String] = None

//  private object GetSig extends MethodSig:
//    def upon: Set[SymbolType] = Set(SymbolType.Map)
//    def out(r: SymbolType): SymbolType = SymbolType.Map

  private object StartsWithSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  private object EndsWithSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  private object ContainsSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar, SymbolType.List, SymbolType.OptionalList, SymbolType.None, SymbolType.Map, SymbolType.OptionalMap)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  private object EqualsIgnoreCaseSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  private object MatchesRegexSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  private object ElseSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.OptionalScalar, SymbolType.None)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  private object IsDefinedSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.OptionalScalar, SymbolType.OptionalList, SymbolType.OptionalMap, SymbolType.None)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  private object LenSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.List, SymbolType.OptionalList, SymbolType.OptionalMap, SymbolType.None, SymbolType.Scalar, SymbolType.Map)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  private object ToUpperCaseSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  private object ToLowerCaseSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  private object TrimSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  private object TemplateSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  private object SubstrSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  private object ReplaceSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  private object DateFmtSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  private object ToDateSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  private object NowSig extends MethodSig:
    def upon: Set[SymbolType] = Set.empty[SymbolType]
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  private object UuidSig extends MethodSig:
    def upon: Set[SymbolType] = Set.empty[SymbolType]
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  private object SortAscSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.List, SymbolType.OptionalList)
    def out(r: SymbolType): SymbolType = r

  private object SortDescSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.List, SymbolType.OptionalList)
    def out(r: SymbolType): SymbolType = r

  private object FilterSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.List, SymbolType.OptionalList)
    def out(r: SymbolType): SymbolType = r

  private object DistinctSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.List, SymbolType.OptionalList)
    def out(r: SymbolType): SymbolType = r

  private object LimitSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.List, SymbolType.OptionalList)
    def out(r: SymbolType): SymbolType = r

  private object ReverseAscSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.List, SymbolType.OptionalList)
    def out(r: SymbolType): SymbolType = r

  private object CleanSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.List, SymbolType.OptionalList)
    def out(r: SymbolType): SymbolType = r

  private object KeysSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Map, SymbolType.OptionalMap)
    def out(r: SymbolType): SymbolType = SymbolType.List

  private object ValuesSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Map, SymbolType.OptionalMap)
    def out(r: SymbolType): SymbolType = SymbolType.List

  val methodSigs: Map[String, MethodSig] = Map(
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
    M_FILTER -> FilterSig, // parser-side “filter(...)”
    M_DISTINCT -> DistinctSig,
    M_LIMIT -> LimitSig,
    M_REVERSE -> ReverseAscSig,
    M_CLEAN -> CleanSig
  )
