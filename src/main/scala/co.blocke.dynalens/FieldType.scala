package co.blocke.dynalens


sealed trait FieldType:
  def fieldName: String
  def typeName: String
  def isNumeric: Boolean = false
  def isStringLike: Boolean = this match
    case ScalarType(_, "java.lang.String", _)                     => true
    case ListType(_, ScalarType(_, "java.lang.String", _), _, _)  => true
    case ValType(_, inner, _)                                     => inner.isStringLike
    case _                                                        => false
  val isOptional: Boolean

  /** True if this type can be assigned from that type (e.g. Int <- Int, Option[Int] <- Int, etc.) */
  def canAssignTo(other: FieldType): Boolean =
    // --- Step 1: Base shape compatibility (ignores optionality) ---
    val shapeCompatible = (this, other) match
      // numeric widening
      case (ScalarType(_, "scala.Int", _), ScalarType(_, "scala.Long", _)) => true
      case (ScalarType(_, "scala.Int", _), ScalarType(_, "scala.Double", _)) => true
      case (ScalarType(_, "scala.Long", _), ScalarType(_, "scala.Double", _)) => true
      case (ScalarType(_, "scala.Float", _), ScalarType(_, "scala.Double", _)) => true

      // same scalar type
      case (ScalarType(_, tn1, _), ScalarType(_, tn2, _)) => tn1 == tn2

      // list compatibility
      case (ListType(_, e1, _, _), ListType(_, e2, _, _)) => e1.canAssignTo(e2)

      // map compatibility
      case (MapType(_, k1, v1, _, _), MapType(_, k2, v2, _, _)) =>
        k1.canAssignTo(k2) && v1.canAssignTo(v2)

      // class equivalence
      case (ClassType(_, tn1, _, _), ClassType(_, tn2, _, _)) => tn1 == tn2

      // fallback
      case _ => false

    // --- Step 2: Optionality compatibility ---
    // Scalars must match exactly.
    // Collections may differ in optionality (either side can be optional).
    // Optional target can always accept non-optional source.
    val optionalCompatible = (this, other) match
      // Scalars: strict match
      case (_: ScalarType, _: ScalarType) =>
        this.isOptional == other.isOptional

      // Collections: allow optional either side
      case (_: ListType, _: ListType) |
           (_: MapType, _: MapType) =>
        true

      // Optional target may accept non-optional source (general fallback)
      case _ =>
        !this.isOptional || this.isOptional == other.isOptional

    shapeCompatible && optionalCompatible

  def cloneWithOptional(flag: Boolean): FieldType = this match
    case s: ScalarType      => s.copy(isOptional = flag)
    case l: ListType        => l.copy(isOptional = flag)
    case m: MapType         => m.copy(isOptional = flag)
    case c: ClassType       => c.copy(isOptional = flag)
    case v: ValType         => v.copy(valueType = v.valueType.cloneWithOptional(flag))



object FieldType:
  /**
   * Create a synthetic FieldType to represent a transient or computed receiver.
   * Keeps structure/type information but clears the name so it won’t collide with schema fields.
   */
  def synthetic(base: FieldType): FieldType = base match
    case v: ValType =>
      // unwrap the value type and synthesize recursively so `ValType` wrapper stays consistent
      v.copy(fieldName = "", valueType = synthetic(v.valueType))
    case s: ScalarType =>
      s.copy(fieldName = "")
    case l: ListType =>
      l.copy(fieldName = "")
    case m: MapType =>
      m.copy(fieldName = "")
    case c: ClassType =>
      c.copy(fieldName = "")



case class ScalarType(fieldName: String, typeName: String, isOptional: Boolean = false) extends FieldType:
  override def isNumeric: Boolean =
    Set(
      "scala.Byte", "scala.Short", "scala.Int", "scala.Long", "scala.Float", "scala.Double",
      "java.lang.Byte", "java.lang.Short", "java.lang.Integer", "java.lang.Long",
      "java.lang.Float", "java.lang.Double", "scala.math.BigDecimal"
    ).contains(typeName)

case class ListType(fieldName: String, elementType: FieldType, typeName: String, isOptional: Boolean = false) extends FieldType
case class MapType(fieldName: String, keyType: FieldType, valueType: FieldType, typeName: String, isOptional: Boolean = false) extends FieldType
case class ClassType(
                      fieldName: String,
                      typeName: String,
                      fields: List[FieldType],
                      isOptional: Boolean = false
                    ) extends FieldType
case class ValType(fieldName: String, valueType: FieldType, typeName: String) extends FieldType:
  override def isNumeric: Boolean = valueType.isNumeric
  override def isStringLike: Boolean = valueType.isStringLike
  val isOptional: Boolean = valueType.isOptional

/**
 * ResolvedType exiss to carry the end result of Schema.resolvePath(path) — i.e. when you walk a schema through nested FieldTypes (e.g. "foo.bar.baz"), you get:
 * •	the final FieldType of the resolved field, and
 * •	how many optional wrappers (Option[...]) were encountered along that path.
 */
case class ResolvedType(
                         fieldType: FieldType,
                         optionalDepth: Int = 0
                       ):
  def isOptional: Boolean =
    fieldType.isOptional || optionalDepth > 0
  def scalaType: String =
    val base =
      fieldType match
        case ListType(_, element, _, _) =>
          s"List[${element.typeName}]"
        case MapType(_, key, value, _, _) =>
          s"Map[${key.typeName}, ${value.typeName}]"
        case other => other.typeName

    val totalOptional = optionalDepth + (if fieldType.isOptional then 1 else 0)
    if totalOptional > 0 then s"Option[$base]" else base


val NoneFieldType: FieldType = ScalarType("", "scala.Any", isOptional = true)


