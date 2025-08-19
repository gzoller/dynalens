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


  private object StartsWithSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  private object EndsWithSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  private object ContainsSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
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

  private object FilterAscSig extends MethodSig:
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

  val methodSigs: Map[String, MethodSig] = Map(
    // scalar/string ops
    "startsWith"       -> StartsWithSig,
    "endsWith"         -> EndsWithSig,
    "contains"         -> ContainsSig,
    "equalsIgnoreCase" -> EqualsIgnoreCaseSig,
    "matchesRegex"     -> MatchesRegexSig,
    "toUpperCase"      -> ToUpperCaseSig,
    "toLowerCase"      -> ToLowerCaseSig,
    "trim"             -> TrimSig,
    "template"         -> TemplateSig,
    "substr"           -> SubstrSig,
    "replace"          -> ReplaceSig,
    "dateFmt"          -> DateFmtSig,
    "toDate"           -> ToDateSig,
    "now"              -> NowSig,
    "uuid"             -> UuidSig,

    // option/none helpers
    "else"             -> ElseSig,
    "isDefined"        -> IsDefinedSig,

    // generic length (per your Sig)
    "len"              -> LenSig,

    // collection methods
    "sortAsc"          -> SortAscSig,
    "sortDesc"         -> SortDescSig,
    "filter"           -> FilterAscSig,   // parser-side “filter(...)”
    "distinct"         -> DistinctSig,
    "limit"            -> LimitSig,
    "reverse"          -> ReverseAscSig,
    "clean"            -> CleanSig
  )


// TODO: Test argument type checking: foo.filter( items ) -- where items is a List or other non-boolean