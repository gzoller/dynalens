package co.blocke.dynalens

import zio.*


sealed trait Lens {
  def name: String
  def isOptional: Boolean
  def parent: Option[Lens]

  def get(path: List[PathElement], obj: Any): ZIO[Any, DynaLensError, Any]
  def update(path: List[PathElement], value: Any, obj: Any): ZIO[Any, DynaLensError, Any]
}


final case class ClassLens(
                            name: String,
                            isOptional: Boolean,
                            parent: Option[Lens],
                            fields: Map[String, Lens],
                            _get: (String, Any) => ZIO[Any, DynaLensError, Any],
                            _update: (String, Any, Any) => ZIO[Any, DynaLensError, Any]
                          ) extends Lens:

  override def get(path: List[PathElement], obj: Any): ZIO[Any, DynaLensError, Any] =
    path match
      case Nil =>
        ZIO.succeed(obj)

      case PathElement(fieldNameOpt, indexOpt) :: rest =>
        // If this is an optional class lens, unwrap before descending
        val baseObjZ: ZIO[Any, DynaLensError, Any] =
          if isOptional then
            obj match
              case Some(v) => ZIO.succeed(v)
              case None    => ZIO.succeed(None)
              case other   => ZIO.succeed(other)
          else
            obj match
              case None => ZIO.fail(DynaLensError("", s"Required class '$name' cannot be None"))
              case v    => ZIO.succeed(v)

        for
          baseObj <- baseObjZ
          fieldValue <- _get(fieldNameOpt.getOrElse(""), baseObj)
          fieldLens <- ZIO.fromOption(fields.get(fieldNameOpt.getOrElse("")))
            .orElseFail(DynaLensError("", s"No such field: ${fieldNameOpt.getOrElse("")}"))
          result <- indexOpt match
            case Some(idx) =>
              // Field name is consumed; forward only the index
              fieldLens.get(PathElement(None, Some(idx)) :: rest, fieldValue)
            case None =>
              fieldLens.get(rest, fieldValue)
        yield result

  override def update(path: List[PathElement], value: Any, obj: Any): ZIO[Any, DynaLensError, Any] =
    path match
      case Nil =>
        ZIO.succeed(value)

      case PathElement(fieldNameOpt, indexOpt) :: rest =>
        val baseObjZ: ZIO[Any, DynaLensError, Any] =
          if isOptional then
            obj match
              case Some(v) => ZIO.succeed(v)
              case None    => ZIO.succeed(None) // no-op
          else
            if obj == null then
              ZIO.fail(DynaLensError("", s"Cannot descend into null for required class '$name'"))
            else
              ZIO.succeed(obj)

        for
          baseObj <- baseObjZ

          // SHORT-CIRCUIT: skip the rest of update entirely
          updatedBase <-
            if isOptional && baseObj == None then
              ZIO.succeed(None)
            else
              for
                fieldValue <- _get(fieldNameOpt.getOrElse(""), baseObj)
                fieldLens  <- ZIO.fromOption(fields.get(fieldNameOpt.getOrElse("")))
                  .orElseFail(DynaLensError("", s"No such field: ${fieldNameOpt.getOrElse("")}"))
                updatedField <- indexOpt match
                  case Some(idx) =>
                    fieldLens.update(PathElement(None, Some(idx)) :: rest, value, fieldValue)
                  case None =>
                    fieldLens.update(rest, value, fieldValue)

                finalFieldValue <-
                  if !fieldLens.isOptional && updatedField.isInstanceOf[Option[?]] then
                    ZIO.fail(DynaLensError("", s"Cannot assign Option to non-optional field '${fieldLens.name}'"))
                  else
                    ZIO.succeed(
                      if fieldLens.isOptional && !updatedField.isInstanceOf[Option[?]] then
                        Some(updatedField)
                      else
                        updatedField
                    )

                newObj <-
                  ZIO
                    .attempt(_update(fieldNameOpt.getOrElse(""), finalFieldValue, baseObj))
                    .flatten
                    .sandbox
                    .foldZIO(
                      cause =>
                        val classCastInDefects = cause.defects.collectFirst { case e: ClassCastException => e }
                        val classCastInFailure = cause.failureOption.collect { case e: ClassCastException => e }
                        val classCastOpt = classCastInDefects.orElse(classCastInFailure)

                        classCastOpt match
                          case Some(_) =>
                            val attemptedType = if finalFieldValue == null then "null" else finalFieldValue.getClass.getName
                            val expectedType  = if fieldValue == null then "unknown" else fieldValue.getClass.getName
                            ZIO.fail(
                              DynaLensError(
                                "",
                                s"Cannot assign value of type $attemptedType to field '${fieldNameOpt.getOrElse("")}' ($expectedType) of class '$name'"
                              )
                            )
                          case None =>
                            cause.failureOption match
                              case Some(e) =>
                                ZIO.fail(DynaLensError("", s"Unexpected update error on class '$name': ${e.getMessage}"))
                              case None =>
                                ZIO.fail(DynaLensError("", s"Unexpected defect during update on class '$name': ${cause.prettyPrint}"))
                      ,
                      updated => ZIO.succeed(updated)
                    )
              yield newObj

          result <- if isOptional then ZIO.succeed(Some(updatedBase)) else ZIO.succeed(updatedBase)
        yield result


final case class ListLens(
                           name: String,
                           isOptional: Boolean,
                           elementLens: Lens,
                           parent: Option[Lens]
                         ) extends Lens:

  override def get(path: List[PathElement], obj: Any): ZIO[Any, DynaLensError, Any] =
    path match
      case Nil =>
        ReflectUtil.unwrapOptionList(obj, isOptional)

      case PathElement(_, indexOpt) :: rest =>
        val listZ = ReflectUtil.unwrapOptionList(obj, isOptional)
        indexOpt match
          case Some(idxStr) =>
            val idx = idxStr.toIntOption.getOrElse(-1)
            listZ.flatMap { list =>
              if idx < 0 || idx >= list.size then
                ZIO.fail(DynaLensError("", s"Index $idx out of bounds for list '$name'"))
              else
                elementLens.get(rest, list(idx))
            }
          case None =>
            // No index given → return the list itself
            listZ

  override def update(path: List[PathElement], value: Any, obj: Any): ZIO[Any, DynaLensError, Any] =
    path match
      case Nil =>
        if isOptional then
          obj match
            case None => ZIO.succeed(Nil)
            case Some(list: List[?]) =>
              if list.isEmpty then ZIO.succeed(Nil)
              else ZIO.succeed(list)
            case other =>
              ZIO.succeed(other)
        else
          obj match
            case None => ZIO.fail(DynaLensError("", s"Required list '$name' cannot be None"))
            case _ => ZIO.succeed(obj)

      case PathElement(_, indexOpt) :: rest =>
        val listZ = ReflectUtil.unwrapOptionList(obj, isOptional)

        indexOpt match
          case Some(idxStr) =>
            val idx = idxStr.toIntOption.getOrElse(-1)
            listZ.flatMap { list =>
              if idx < 0 || idx >= list.size then
                if isOptional then
                  // Optional list missing → no-op, remain None
                  ZIO.succeed(obj)
                else
                  // Required → fail
                  ZIO.fail(DynaLensError("", s"Index $idx out of bounds for list '$name'"))
              else
                for
                  updatedElem <- elementLens.update(rest, value, list(idx))
                  newObj <- ReflectUtil.setListElem(obj, idx, updatedElem, isOptional)
                yield newObj
            }
          case None =>
            ZIO.fail(DynaLensError("", s"No index specified for list '$name' update"))


final case class MapLens(
  name: String,
  isOptional: Boolean,
  keyKind: MapKeyKind,
  valueLens: Lens,
  parent: Option[Lens]
) extends Lens:

  override def get(path: List[PathElement], obj: Any): ZIO[Any, DynaLensError, Any] =
    path match
      case Nil =>
        ReflectUtil.unwrapOptionMap[Any, Any](obj, isOptional)

      case PathElement(_, indexOpt) :: rest =>
        val mapZ = ReflectUtil.unwrapOptionMap[Any, Any](obj, isOptional)
        indexOpt match
          case Some(keyStr) =>
            for
              typedKey <- keyKind match
                case MapKeyKind.StringKey => ZIO.succeed(keyStr)
                case MapKeyKind.IntKey =>
                  ZIO.fromOption(keyStr.toIntOption)
                    .orElseFail(DynaLensError("", s"Invalid Int key '$keyStr' for map '$name'"))
                case MapKeyKind.LongKey =>
                  ZIO.attempt(keyStr.toLong)
                    .mapError(_ => DynaLensError("", s"Invalid Long key '$keyStr' for map '$name'"))
                case MapKeyKind.EnumKey(enumName) =>
                  ZIO.attempt(ReflectUtil.coerceEnumKey(keyStr, enumName))
                    .mapError(e => DynaLensError("", s"Invalid Enum key '$keyStr' for enum '$enumName' in map '$name': ${e.getMessage}"))
              map <- mapZ
              res <- map.get(typedKey) match
                case Some(null) =>
                  // Treat explicit null as missing
                  ZIO.succeed(None)
                case Some(v) =>
                  // Always delegate to the value lens, even when rest is Nil, so that
                  // scalar/enum normalization is applied consistently.
                  valueLens.get(rest, v)
                case None =>
                  // Missing key -> return None (do not fail here)
                  ZIO.succeed(None)
            yield res

          case None =>
            mapZ

  override def update(path: List[PathElement], value: Any, obj: Any): ZIO[Any, DynaLensError, Any] =
    path match
      case Nil =>
        ZIO.succeed(value)
      case PathElement(_, indexOpt) :: rest =>
        val mapZ = ReflectUtil.unwrapOptionMap[Any, Any](obj, isOptional)
        indexOpt match
          case Some(keyStr) =>
            for {
              typedKey <- (keyKind match
                case MapKeyKind.StringKey => ZIO.succeed(keyStr)
                case MapKeyKind.IntKey =>
                  ZIO.fromOption(keyStr.toIntOption)
                    .orElseFail(DynaLensError("", s"Invalid Int key '$keyStr' for map '$name'"))
                case MapKeyKind.LongKey =>
                  ZIO.attempt(keyStr.toLong)
                    .mapError(_ => DynaLensError("", s"Invalid Long key '$keyStr' for map '$name'"))
                case MapKeyKind.EnumKey(enumName) =>
                  ZIO.attempt(ReflectUtil.coerceEnumKey(keyStr, enumName))
                    .mapError(e => DynaLensError("", s"Invalid Enum key '$keyStr' for enum '$enumName' in map '$name': ${e.getMessage}"))
              )
              map <- mapZ
              updatedObj <- (map.get(typedKey) match {
                case None if rest.nonEmpty =>
                  if isOptional then ZIO.succeed(obj)
                  else ZIO.fail(DynaLensError("", s"Key '$keyStr' not found in map '$name'"))
                case None =>
                  ZIO.succeed {
                    val newMap = map + (typedKey -> value)
                    if isOptional then Some(newMap) else newMap
                  }
                case Some(currentValue) =>
                  for {
                    updatedValue <- valueLens.update(rest, value, currentValue)
                    newObj <- ReflectUtil.setMapValue(obj, keyStr, keyKind, updatedValue, isOptional)
                  } yield newObj
              })
            } yield updatedObj
          case None =>
            ZIO.fail(DynaLensError("", s"No key specified for map '$name' update"))


final case class ScalarLens(
  name: String,
  isOptional: Boolean,
  parent: Option[Lens]
) extends Lens:

  override def get(path: List[PathElement], obj: Any): ZIO[Any, DynaLensError, Any] =
    path match
      case Nil =>
        ZIO.succeed(obj)
      case _ =>
        ZIO.fail(DynaLensError("", s"Cannot descend further into scalar value '$name'"))

  override def update(path: List[PathElement], value: Any, obj: Any): ZIO[Any, DynaLensError, Any] =
    path match
      case Nil =>
        ZIO.succeed(value)
      case _ =>
        ZIO.fail(DynaLensError("", s"Cannot descend further into scalar value '$name'"))


final case class EnumLens(
                           name: String,
                           isOptional: Boolean,
                           enumClassName: String,
                           parent: Option[Lens]
                         ) extends Lens:
  override def get(path: List[PathElement], obj: Any): ZIO[Any, DynaLensError, Any] =
    path match
      case Nil =>
        ZIO.succeed(convertEnumToString(obj))
      case _ =>
        ZIO.fail(DynaLensError("",s"Cannot descend into enum '$name'"))

  override def update(path: List[PathElement], value: Any, obj: Any): ZIO[Any, DynaLensError, Any] =
    path match
      case Nil =>
        ZIO.succeed(convertStringToEnum(value.toString))
      case _ =>
        ZIO.fail(DynaLensError("",s"Cannot descend into enum '$name'"))

  private def convertEnumToString(obj: Any): String =
    obj.toString.takeWhile(_ != '(') // clean Scala3 default format

  private def convertStringToEnum(s: String): Any =
    val enumClass = Class.forName(enumClassName)
    enumClass.getMethod("valueOf", classOf[String]).invoke(null, s)