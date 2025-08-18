package co.blocke.dynalens.parser

object Methods:

  trait MethodSig:
    /** Accepted receiver kinds (coarse). E.g., Set(List) for filter/sort. */
    def upon: Set[SymbolType]

    /** Result kind given the receiver kind (coarse). */
    def out(receiver: SymbolType): SymbolType

  /** (Optional) validate args against receiver kind; return message when invalid. */
  //    def validateArgs(receiver: SymbolType, args: List[Fn[?]]): Option[String] = None


  object StartsWithSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  object EndsWithSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  object ContainsSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  object EqualsIgnoreCaseSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  object MatchesRegexSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  object ElseSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.OptionalScalar, SymbolType.None)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  object IsDefinedSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.OptionalScalar, SymbolType.OptionalList, SymbolType.OptionalMap, SymbolType.None)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  object LenSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.List, SymbolType.OptionalList, SymbolType.OptionalMap, SymbolType.None, SymbolType.Scalar, SymbolType.Map)
    def out(r: SymbolType): SymbolType = SymbolType.Boolean

  object ToUpperCaseSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  object ToLowerCaseSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  object TrimSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  object TemplateSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  object SubstrSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  object ReplaceSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  object DateFmtSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  object ToDateSig extends MethodSig:
    def upon: Set[SymbolType] = Set(SymbolType.Scalar)
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  object NowSig extends MethodSig:
    def upon: Set[SymbolType] = Set.empty[SymbolType]
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

  object UuidSig extends MethodSig:
    def upon: Set[SymbolType] = Set.empty[SymbolType]
    def out(r: SymbolType): SymbolType = SymbolType.Scalar

//
//  val methodSigs: Map[String, MethodSig] = Map(
//    "filter" -> FilterSig,
//    "sortAsc" -> SortAscSig,
//    "len" -> LengthSig,
//    "isDefined" -> IsDefinedSig,
//    "equalsIgnoreCase" -> EqualsIgnoreCaseSig,
//    // ...
//  )

// TODO: Test argument type checking: foo.filter( items ) -- where items is a List or other non-boolean