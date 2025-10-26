package co.blocke.dynalens

import zio.*


object ReflectUtil:

  /**
   * Unwraps an Option[List[Any]] if isOptional is true, or casts obj to List[Any] if not.
   * Returns Some(List[Any]) if successful, None otherwise.
   */
  def unwrapOptionList(obj: Any, isOptional: Boolean): ZIO[Any, DynaLensError, List[Any]] =
    obj match
      case l: List[?] => ZIO.succeed(l)
      case Some(l: List[?]) => ZIO.succeed(l)
      case None | null if isOptional => ZIO.succeed(Nil)
      case None | null => ZIO.fail(DynaLensError("", s"Null or None for non-optional List"))
      case other =>
        ZIO.fail(DynaLensError("", s"Expected List or Option[List], got ${other.getClass.getSimpleName}"))

  /**
   * Unwraps an Option[Map[K,V]] if isOptional is true, or casts obj to Map[K,V] if not.
   * Returns Some(Map[K,V]) if successful, None otherwise.
   */
  def unwrapOptionMap[K, V](obj: Any, isOptional: Boolean): ZIO[Any, DynaLensError, Map[K, V]] =
    obj match
      case m: Map[?, ?] => ZIO.succeed(m.asInstanceOf[Map[K, V]])
      case Some(m: Map[?, ?]) => ZIO.succeed(m.asInstanceOf[Map[K, V]])
      case None | null if isOptional => ZIO.succeed(Map.empty[K, V])
      case None | null => ZIO.fail(DynaLensError("", s"Null or None for non-optional Map"))
      case other =>
        ZIO.fail(DynaLensError("", s"Expected Map or Option[Map], got ${other.getClass.getSimpleName}"))

  /**
   * Generic field/key lookup for UpdateStmt navigation.
   * - If PathElement has a field name → reflective getter
   * - If it has a key → route to appropriate Map/List helpers
   */
  def getValue(current: Any, elem: PathElement): ZIO[Any, DynaLensError, Any] =
    if current == null then
      ZIO.fail(DynaLensError("", s"Cannot traverse null parent"))
    else
      elem match
        case PathElement(Some(field), None) =>
          // Reflective field getter
          ZIO
            .attempt {
              val method = current.getClass.getMethod(field)
              method.invoke(current)
            }
            .mapError(e => DynaLensError("", s"Field '$field' not found on ${current.getClass.getSimpleName}: ${e.getMessage}"))

        case PathElement(None, Some(keyStr)) =>
          // All map/list keys handled as String; coercion happens downstream
          getMapValue(current, keyStr, MapKeyKind.StringKey, isOptional = true)

        case _ =>
          ZIO.fail(DynaLensError("", s"Invalid path element: $elem"))
  
  /**
   * Returns a new list with the element at idx replaced by newValue.
   * If isOptional is true, wraps the new list in Some.
   * Handles immutability by creating a new list.
   */
  def setListElem(obj: Any, idx: Int, newValue: Any, isOptional: Boolean): ZIO[Any, DynaLensError, Any] =
    for
      list <- unwrapOptionList(obj, isOptional)
      updated <-
        if idx < 0 || idx >= list.size then
          ZIO.fail(DynaLensError("", s"Index $idx out of bounds for list"))
        else
          ZIO.succeed(list.updated(idx, newValue))
    yield if isOptional then Some(updated) else updated

  def getListElem(obj: Any, idx: Int, isOptional: Boolean): ZIO[Any, DynaLensError, Any] =
    for
      list <- unwrapOptionList(obj, isOptional)
      result <-
        if idx < 0 || idx >= list.size then
          ZIO.fail(DynaLensError("", s"Index $idx out of bounds"))
        else
          ZIO.succeed(list(idx))
    yield result

  def coerceKey(keyStr: String, keyType: Class[?]): Any =
    if keyType == classOf[String] then keyStr
    else if keyType == classOf[Int] then keyStr.toInt
    else if keyType == classOf[Long] then keyStr.toLong
    else if keyType == classOf[Boolean] then keyStr.toBoolean
    else if keyType.isEnum then
      val method = keyType.getMethod("valueOf", classOf[String])
      method.invoke(null, keyStr)
    else
      throw new IllegalArgumentException(s"Unsupported Map key type: ${keyType.getName}")

  def getMapValue(obj: Any, keyStr: String, keyKind: MapKeyKind, isOptional: Boolean): ZIO[Any, DynaLensError, Any] =
    for
      map <- unwrapOptionMap[Any, Any](obj, isOptional)
      typedKey = keyKind match
        case MapKeyKind.StringKey => keyStr
        case MapKeyKind.IntKey => keyStr.toInt
        case MapKeyKind.LongKey => keyStr.toLong
        case MapKeyKind.EnumKey(name) => coerceEnumKey(keyStr, name)
      value <- ZIO.succeed(map.asInstanceOf[Map[Any, Any]].getOrElse(typedKey, null))
    yield value

  /**
   * Returns a new map with the value at key replaced by newValue.
   * If isOptional is true, wraps the new map in Some.
   * Handles immutability by creating a new map.
   */
  def setMapValue(obj: Any, keyStr: String, keyKind: MapKeyKind, newValue: Any, isOptional: Boolean): ZIO[Any, DynaLensError, Any] =
    for
      map <- unwrapOptionMap[Any, Any](obj, isOptional)
      typedKey = keyKind match
        case MapKeyKind.StringKey => keyStr
        case MapKeyKind.IntKey => keyStr.toInt
        case MapKeyKind.LongKey => keyStr.toLong
        case MapKeyKind.EnumKey(enumName) => coerceEnumKey(keyStr, enumName)
      newMap = map.updated(typedKey, newValue)
    yield if isOptional then Some(newMap) else newMap

  /**
   * Coerces a string key into an enum constant of the given fully-qualified enum name.
   */
  def coerceEnumKey(keyStr: String, enumName: String): Any =
    try
      val clazz = Class.forName(enumName)
      val method = clazz.getMethod("valueOf", classOf[String])
      method.invoke(null, keyStr)
    catch
      case _: ClassNotFoundException =>
        throw new IllegalArgumentException(s"Enum class not found: $enumName")
      case _: NoSuchMethodException =>
        throw new IllegalArgumentException(s"Enum $enumName does not define valueOf(String)")
      case e: Exception =>
        throw new IllegalArgumentException(s"Failed to coerce '$keyStr' into enum $enumName: ${e.getMessage}")

  def setScalarValue(newValue: Any, isOptional: Boolean): ZIO[Any, DynaLensError, Any] =
    ZIO.succeed {
      if isOptional then
        newValue match
          case o: Option[?] => o // already wrapped
          case null => None // null → None
          case v => Some(v) // auto-wrap raw values
      else
        newValue
    }