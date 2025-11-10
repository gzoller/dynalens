package co.blocke.dynalens

import scala.annotation.tailrec
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.RTypeRef


object Schema:

  private def markOptional(ft: FieldType): FieldType = ft match
    case s: ScalarType       => s.copy(isOptional = true)
    case c: ClassType        => c.copy(isOptional = true)
    case l: ListType         => l.copy(isOptional = true)
    case m: MapType          => m.copy(isOptional = true)
    case e: EnumType         => e.copy(isOptional = true)
    case vt: ValType         => vt.copy(valueType = markOptional(vt.valueType))

  /** Build a full ClassType tree for a ScalaClassRef */
  def build(ref: RTypeRef[?]): ClassType = ref match
    case sc: ScalaClassRef[?] =>
      ClassType(
        name = "",                      // root has no parent field name
        typeName  = ref.name,
        fields    = sc.fields.map(f => fieldTypeFromRTypeRef(f.name, f.fieldRef))
      )
    case other =>
      throw new UnsupportedOperationException(
        s"Schema.build() supports only case classes, not ${other.getClass.getSimpleName}"
      )

  /** Resolve a dotted path inside a ClassType tree. */
  def resolvePath(root: ClassType, path: String): Option[ResolvedType] =
    resolvePath(root, path.split('.').toList, optDepth = 0)

  @tailrec
  private def resolvePath(node: ClassType, segments: List[String], optDepth: Int): Option[ResolvedType] =
    segments match
      case Nil => None
      case head :: tail =>
        node.fields.find(_.name == head) match
          case Some(ft) =>
            ft match
              case ft if ft.isOptional =>
                resolveOptionalField(ft, tail, optDepth)
              case c: ClassType =>
                if tail.isEmpty then Some(ResolvedType(c, optDepth))
                else resolvePath(c, tail, optDepth)
              case l: ListType =>
                resolveList(l, tail, optDepth)
              case m: MapType =>
                resolveMap(m, tail, optDepth)
              case other =>
                if tail.isEmpty then Some(ResolvedType(other, optDepth)) else None
          case None => None

  // --- helper resolvers, each working with inlined fields ---
  private def resolveOptionalField(ft: FieldType, tail: List[String], optDepth: Int): Option[ResolvedType] =
    val nextDepth = optDepth + 1
    ft match
      case c: ClassType =>
        if tail.isEmpty then Some(ResolvedType(c.copy(isOptional = true), nextDepth))
        else resolvePath(c, tail, nextDepth)

      case l: ListType =>
        if tail.isEmpty then Some(ResolvedType(l.copy(isOptional = true), nextDepth))
        else resolveList(l, tail, nextDepth)

      case m: MapType =>
        if tail.isEmpty then Some(ResolvedType(m.copy(isOptional = true), nextDepth))
        else resolveMap(m, tail, nextDepth)

      case other =>
        if tail.isEmpty then Some(ResolvedType(markOptional(other), nextDepth))
        else None


  private def resolveList(l: ListType, tail: List[String], optDepth: Int): Option[ResolvedType] =
    tail match
      case Nil =>
        Some(ResolvedType(l, optDepth))
      case _ =>
        l.elementType match
          case c: ClassType => resolvePath(c, tail, optDepth)
          case other => if tail.isEmpty then Some(ResolvedType(other, optDepth)) else None

  private def resolveMap(m: MapType, tail: List[String], optDepth: Int): Option[ResolvedType] =
    tail match
      case "key" :: Nil   => Some(ResolvedType(m.keyType, optDepth))
      case "value" :: st  => resolvePath(
        m.valueType.asInstanceOf[ClassType], st, optDepth
      )
      case _              => if tail.isEmpty then Some(ResolvedType(m, optDepth)) else None

  private def fieldTypeFromRTypeRef(fieldName: String, ref: RTypeRef[?], isOptional: Boolean = false): FieldType = ref match
    // --- Primitive scalars ---
    case p: PrimitiveRef =>
      ScalarType(fieldName, p.typedName.toString, isOptional)

    // --- Option[T] ---
    case o: OptionRef[?] =>
      fieldTypeFromRTypeRef(fieldName, o.optionParamType, true)

    // --- Seq[T] / List[T] / Array[T] ---
    case s: SeqRef[?] =>
      val elem = fieldTypeFromRTypeRef(fieldName, s.elementRef)
      ListType(
        name = fieldName,
        elementType = elem,
        typeName = s.typedName.toString, // "scala.List", "scala.Seq", "scala.Array",
        isOptional = isOptional
      )

    // --- Map[K,V] ---
    case m: MapRef[?] =>
      val key = fieldTypeFromRTypeRef(fieldName, m.elementRef)
      val value = fieldTypeFromRTypeRef(fieldName, m.elementRef2)
      MapType(
        name = fieldName,
        keyType = key,
        valueType = value,
        typeName = m.typedName.toString, // "scala.collection.immutable.Map", etc.
        isOptional = isOptional
      )

    // ---- Traits
    case t: TraitRef[?] =>
      // Open trait: refuse or stub out
      // (Throwing exception here is ok--this is called from macro during compilation, not runtime)
      throw new UnsupportedOperationException(
        s"DynaLens Schema does not support traits: ${t.name}"
      )

    // --- Nested classes (still stubbed for now) ---
    case sc: ScalaClassRef[?] =>
      // Build inline schema for this class
      val fieldTypes: List[FieldType] =
        sc.fields.map(f => fieldTypeFromRTypeRef(f.name, f.fieldRef))

      ClassType(
        name = fieldName,
        typeName = sc.name,
        fields   = fieldTypes,
        isOptional = isOptional
      )

    // --- EnumRef[?] ---
    case e: EnumRef[?] =>
      EnumType(
        name = fieldName,
        validValues = e.values,
        typeName = e.name,
        isOptional = isOptional
      )

    case _: SelfRefRef[?] =>
      ScalarType(fieldName, "SelfRef[?]", isOptional)

    // fallback
    case other =>
      ScalarType(fieldName, s"Unhandled(${other.getClass.getSimpleName})", isOptional)
