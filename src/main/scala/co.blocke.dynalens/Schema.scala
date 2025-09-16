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
case class ClassType(name: String, typeName: String) extends FieldType:
  def resolve(master: Schema): Schema = master.catalog(typeName)
case class ParamClassType(
                           name: String,
                           typeName: String, // Fully qualified, like "Wrapper[Int]"
                           schema: Schema // Fully resolved, **inlined**
                         ) extends FieldType
case class SealedTraitType(name: String,
                           subTypes: List[ClassType],
                           typeName: String) extends FieldType

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


case class Schema(
                   className: String,
                   fields: List[FieldType],
                   catalog: Map[String, Schema]
                 ):
  def resolvePath(path: String): Option[ResolvedType] =
    resolvePath(path.split('.').toList, 0)

  def resolvePath(path: List[String], optDepth: Int): Option[ResolvedType] = path match
    case Nil => None

    case head :: Nil =>
      fields.find(_.name == head) match
        // 👇 If the leaf itself is Option[T], unwrap to T and bump optional depth
        case Some(ot: OptionType) =>
          Some(ResolvedType(ot.valueType, optDepth + 1))

        case Some(ft) =>
          Some(ResolvedType(ft, optDepth))

        case None =>
          // no synthetic .key/.value at leaf level
          None

    case head :: tail =>
      fields.find(_.name == head) match
        case Some(ft) =>
          ft match
            // ---- Option[T] ----
            case ot: OptionType =>
              ot.valueType match
                case ct: ClassType =>
                  catalog.get(ct.typeName).flatMap { sub =>
                    Schema(ct.typeName, sub.fields, catalog).resolvePath(tail, optDepth + 1)
                  }
                case pc: ParamClassType =>
                  Schema(pc.schema.className, pc.schema.fields, catalog).resolvePath(tail, optDepth + 1)
                case inner =>
                  // Not a class-like thing; keep going only if the tail is empty (otherwise it's a dead end).
                  if tail.isEmpty then Some(ResolvedType(inner, optDepth + 1)) else None

            // ---- Class ----
            case ct: ClassType =>
              catalog.get(ct.typeName).flatMap { sub =>
                Schema(ct.typeName, sub.fields, catalog).resolvePath(tail, optDepth)
              }

            // ---- ParamClass (already inlined schema) ----
            case pc: ParamClassType =>
              Schema(pc.schema.className, pc.schema.fields, catalog).resolvePath(tail, optDepth)

            // ---- SealedTrait ----
            case st: SealedTraitType =>
              // The path must specify which concrete subtype to descend into.
              // We expect the next segment to match one of the known ClassType names.
              tail match
                case subHead :: subTail =>
                  st.subTypes.find(_.typeName.endsWith(subHead)) match
                    case Some(classType) =>
                      catalog.get(classType.typeName)
                        .flatMap(_.resolvePath(subTail, optDepth))
                    case None =>
                      None
                case Nil =>
                  // If no subtype is specified, treat the trait itself as the end of the path
                  Some(ResolvedType(st, optDepth))

            // ---- List[T] ----
            case lt: ListType =>
              lt.elementType match
                case ct: ClassType =>
                  catalog.get(ct.typeName).flatMap { sub =>
                    Schema(ct.typeName, sub.fields, catalog).resolvePath(tail, optDepth)
                  }
                case pc: ParamClassType =>
                  Schema(pc.schema.className, pc.schema.fields, catalog).resolvePath(tail, optDepth)
                case elem =>
                  // Treat as terminal unless you later add explicit []/index semantics.
                  if tail.isEmpty then Some(ResolvedType(elem, optDepth)) else None

            // ---- Map[K,V] ----
            case mt: MapType =>
              tail match
                case "key" :: Nil =>
                  Some(ResolvedType(mt.keyType, optDepth))

                case "value" :: valueTail =>
                  mt.valueType match
                    case ot: OptionType =>
                      ot.valueType match
                        case ct: ClassType =>
                          catalog.get(ct.typeName).flatMap { sub =>
                            Schema(ct.typeName, sub.fields, catalog).resolvePath(valueTail, optDepth + 1)
                          }
                        case pc: ParamClassType =>
                          Schema(pc.schema.className, pc.schema.fields, catalog).resolvePath(valueTail, optDepth + 1)
                        case inner =>
                          if valueTail.isEmpty then Some(ResolvedType(inner, optDepth + 1)) else None

                    case ct: ClassType =>
                      catalog.get(ct.typeName).flatMap { sub =>
                        Schema(ct.typeName, sub.fields, catalog).resolvePath(valueTail, optDepth)
                      }

                    case pc: ParamClassType =>
                      Schema(pc.schema.className, pc.schema.fields, catalog).resolvePath(valueTail, optDepth)

                    case other =>
                      if valueTail.isEmpty then Some(ResolvedType(other, optDepth)) else None

                case _ =>
                  // If caller doesn’t ask for .key/.value, we treat the map itself as terminal.
                  Some(ResolvedType(mt, optDepth))

            // ---- Scalar or anything else terminal ----
            case other =>
              if tail.isEmpty then Some(ResolvedType(other, optDepth)) else None

        case None => None


object Schema:
  def buildSchema(ref: ScalaClassRef[?]): Schema =
    val fields: List[FieldType] =
      ref.fields.map(f => fieldTypeFromRTypeRef(f.name, f.fieldRef))

    // Catalog only needs plain classes
    val childSchemas: Map[String, Schema] =
      ref.fields.collect {
        case f if f.fieldRef.isInstanceOf[ScalaClassRef[?]] =>
          val sc = f.fieldRef.asInstanceOf[ScalaClassRef[?]]
          if sc.isAppliedType || sc.typeParamValues.nonEmpty then
            // Applied type: do NOT put into catalog
            None
          else
            Some(sc.name -> buildSchema(sc))
      }.flatten.toMap

    Schema(
      className = ref.name,
      fields = fields,
      catalog = childSchemas
    )

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
      val childSchemas: List[ClassType] =
        t.sealedChildren.collect { case c: ScalaClassRef[?] =>
          ClassType(fieldName, c.name)   // catalog entry will carry full schema
        }

      SealedTraitType(
        name     = fieldName,
        subTypes = childSchemas,
        typeName = t.name
      )

    case t: TraitRef[?] =>
      // Open trait: refuse or stub out
      // (Throwing exception here is ok--this is called from macro during compilation, not runtime)
      throw new UnsupportedOperationException(
        s"DynaLens Schema only supports sealed traits: ${t.name}"
      )

    // --- Nested classes (still stubbed for now) ---
    case sc: ScalaClassRef[?] =>
      if sc.isAppliedType || sc.typeParamValues.nonEmpty then
        // e.g., Wrapper[String] => inline its fully-resolved schema
        ParamClassType(
          name     = fieldName,
          typeName = sc.typedName.toString,   // "Wrapper[java.lang.String]"
          schema   = buildSchema(sc)          // inline the applied class' own schema
        )
      else
        // plain non-generic class reference
        ClassType(
          name     = fieldName,
          typeName = sc.name                   // fully qualified, e.g. "com.foo.Address"
        )

    case _: SelfRefRef[?] =>
      ScalarType(fieldName, "SelfRef[?]")

    // fallback
    case other =>
      ScalarType(fieldName, s"Unhandled(${other.getClass.getSimpleName})")
