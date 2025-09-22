package co.blocke.dynalens

import scala.annotation.tailrec
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.RTypeRef


sealed trait FieldType {
  def name: String
  def typeName: String
}
case class ScalarType(name: String, typeName: String) extends FieldType
case class OptionType(name: String, valueType: FieldType, typeName: String) extends FieldType
case class ListType(name: String, elementType: FieldType, typeName: String) extends FieldType
case class MapType(name: String, keyType: FieldType, valueType: FieldType, typeName: String) extends FieldType
case class ClassType(
                      name: String,
                      typeName: String,
                      fields: List[FieldType]
                    ) extends FieldType
case class SealedTraitType(
                            name: String,
                            typeName: String,
                            fields: List[FieldType],
                            subTypes: List[String]
                          ) extends FieldType

case class ValType(name: String, valueType: FieldType, typeName: String) extends FieldType

case class ResolvedType(
                         fieldType: FieldType,
                         optionalDepth: Int = 0
                       ):
  def scalaType: String =
    @tailrec
    def baseTypeName(ft: FieldType): String = ft match
      case OptionType(_, inner, _) => baseTypeName(inner)
      case other                   => other.typeName

    val base = baseTypeName(fieldType)
    if optionalDepth > 0 then s"Option[$base]" else base


object Schema:

  /** Build a full ClassType tree for a ScalaClassRef */
  def build(ref: RTypeRef[?]): ClassType = ref match
    case sc: ScalaClassRef[?] =>
      ClassType(
        name     = "",                      // root has no parent field name
        typeName = ref.name,
        fields   = sc.fields.map(f => fieldTypeFromRTypeRef(f.name, f.fieldRef))
      )
    case tr: TraitRef[?] if tr.isSealed =>
      val commonFields =
        tr.fields.map(f => fieldTypeFromRTypeRef(f.name, f.fieldRef))

      val subTypes =
        tr.sealedChildren.collect {
          case c: ScalaClassRef[?] if c.isCaseClass => c.typedName.toString
        }

      ClassType(
        name     = tr.name,
        typeName = tr.name,
        fields   = List(
          SealedTraitType(
            name      = tr.name,
            typeName  = tr.name,
            fields    = commonFields,  // shared fields like `name`
            subTypes  = subTypes
          )
        )
      )
    case other =>
      throw new UnsupportedOperationException(
        s"Schema.build() supports only case classes and sealed traits, not ${other.getClass.getSimpleName}"
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
              case o: OptionType =>
                resolveOption(o, tail, optDepth)
              case c: ClassType =>
                if tail.isEmpty then Some(ResolvedType(c, optDepth))
                else resolvePath(c, tail, optDepth)
              case s: SealedTraitType =>
                resolveTrait(s, tail, optDepth)
              case l: ListType =>
                resolveList(l, tail, optDepth)
              case m: MapType =>
                resolveMap(m, tail, optDepth)
              case other =>
                if tail.isEmpty then Some(ResolvedType(other, optDepth)) else None
          case None => None

  // --- helper resolvers, each working with inlined fields ---
  private def resolveOption(o: OptionType, tail: List[String], optDepth: Int): Option[ResolvedType] =
    o.valueType match
      case c: ClassType => resolvePath(c, tail, optDepth + 1)
      case other        => if tail.isEmpty then Some(ResolvedType(other, optDepth + 1)) else None

  private def resolveTrait(s: SealedTraitType, tail: List[String], optDepth: Int): Option[ResolvedType] =
    tail match
      case Nil => Some(ResolvedType(s, optDepth))
      case subHead :: subTail =>
        s.subTypes.find(_.endsWith(subHead)).flatMap { _ =>
          // Find the concrete ClassType already inlined somewhere under root
          s.fields.collectFirst {
            case c: ClassType if c.typeName.endsWith(subHead) =>
              resolvePath(c, subTail, optDepth)
          }.flatten
        }

  private def resolveList(l: ListType, tail: List[String], optDepth: Int): Option[ResolvedType] =
    l.elementType match
      case c: ClassType => resolvePath(c, tail, optDepth)
      case other        => if tail.isEmpty then Some(ResolvedType(other, optDepth)) else None

  private def resolveMap(m: MapType, tail: List[String], optDepth: Int): Option[ResolvedType] =
    tail match
      case "key" :: Nil   => Some(ResolvedType(m.keyType, optDepth))
      case "value" :: st  => resolvePath(
        m.valueType.asInstanceOf[ClassType], st, optDepth
      )
      case _              => if tail.isEmpty then Some(ResolvedType(m, optDepth)) else None

  private def fieldTypeFromRTypeRef(fieldName: String, ref: RTypeRef[?]): FieldType = ref match
    // --- Primitive scalars ---
    case p: PrimitiveRef =>
      ScalarType(fieldName, p.typedName.toString)

    // --- Option[T] ---
    case o: OptionRef[?] =>
      val inner = fieldTypeFromRTypeRef(fieldName, o.optionParamType)
      OptionType(
        name = fieldName,
        valueType = inner,
        typeName = "scala.Option"
      )

    // --- Seq[T] / List[T] / Array[T] ---
    case s: SeqRef[?] =>
      val elem = fieldTypeFromRTypeRef(fieldName, s.elementRef)
      ListType(
        name = fieldName,
        elementType = elem,
        typeName = s.typedName.toString // "scala.List", "scala.Seq", "scala.Array"
      )

    // --- Map[K,V] ---
    case m: MapRef[?] =>
      val key = fieldTypeFromRTypeRef(fieldName, m.elementRef)
      val value = fieldTypeFromRTypeRef(fieldName, m.elementRef2)
      MapType(
        name = fieldName,
        keyType = key,
        valueType = value,
        typeName = m.typedName.toString // "scala.collection.immutable.Map", etc.
      )

    // ---- Traits
    case t: TraitRef[?] if t.isSealed =>
      val commonFields = t.fields.map(f => fieldTypeFromRTypeRef(f.name, f.fieldRef))
      val subTypes = t.sealedChildren.collect { case c: ScalaClassRef[?] => c.typedName.toString }
      SealedTraitType(fieldName, t.name, commonFields, subTypes)

    case t: TraitRef[?] =>
      // Open trait: refuse or stub out
      // (Throwing exception here is ok--this is called from macro during compilation, not runtime)
      throw new UnsupportedOperationException(
        s"DynaLens Schema only supports sealed traits: ${t.name}"
      )

    // --- Nested classes (still stubbed for now) ---
    case sc: ScalaClassRef[?] =>
      // Build inline schema for this class
      val fieldTypes: List[FieldType] =
        sc.fields.map(f => fieldTypeFromRTypeRef(f.name, f.fieldRef))

      ClassType(
        name     = fieldName,
        typeName = sc.name,
        fields   = fieldTypes
      )

    case _: SelfRefRef[?] =>
      ScalarType(fieldName, "SelfRef[?]")

    // fallback
    case other =>
      ScalarType(fieldName, s"Unhandled(${other.getClass.getSimpleName})")