package co.blocke.dynalens
package util

import scala.annotation.tailrec

object PathUtil:

  private def parseCurriedIndexes(seg: String): (String, List[String]) =
    val base = seg.takeWhile(_ != '[')
    val idxPattern = "\\[([^\\]]+)\\]".r
    val indexes = idxPattern.findAllMatchIn(seg).map(_.group(1)).toList
    (base, indexes)

  @tailrec
  private def unwrapContainer(ft: FieldType, index: String): Either[String, FieldType] =
    ft match
      case l: ListType =>
        // list must have numeric index
        if index.forall(_.isDigit) then Right(l.elementType)
        else Left(s"Expected numeric index for List but found '$index'")
      case m: MapType =>
        // we could check key type here, but for now we just return value type
        Right(m.valueType)
      case ft if ft.isOptional =>
        // strip optional and try again
        unwrapContainer(ft.cloneWithOptional(false), index)
      case other =>
        Left(s"Cannot index into non-collection type '${other.typeName}'")

  // NEW: unwrap optional class roots here only
  private def resolveRootType(name: String, schema: FieldType): Either[String, FieldType] =
    schema match
      case c: ClassType =>
        c.fields.find(_.name == name)
          .map(Right(_))
          .getOrElse(Left(s"Unknown root field '$name' in class ${c.typeName}"))
      case ft if ft.isOptional =>
        // only strip optional to reach the class
        resolveRootType(name, ft.cloneWithOptional(false))
      case other =>
        Left(s"Cannot resolve field '$name' on non-class type '${other.typeName}'")

  private def walkSegments(rootType: FieldType, segments: List[String]): Either[String, FieldType] =
    segments.foldLeft[Either[String, FieldType]](Right(rootType)) {
      case (Left(err), _) => Left(err)
      case (Right(current), rawSegment) =>
        val (fieldName, indexes) = parseCurriedIndexes(rawSegment)

        val baseFieldType =
          current match
            case c: ClassType =>
              c.fields.find(_.name == fieldName)
                .map(Right(_))
                .getOrElse(Left(s"Unknown field '$fieldName' in class ${c.typeName}"))
            case _ =>
              Left(s"Cannot traverse into non-class type '${current.typeName}'")

        baseFieldType.flatMap { ft =>
          indexes.foldLeft[Either[String, FieldType]](Right(ft)) { (acc, idx) =>
            acc.flatMap(t => unwrapContainer(t, idx))
          }
        }
    }

  def getPathType(path: String, schema: FieldType): Either[String, FieldType] =
    // empty string case from your test
    if path.trim.isEmpty then
      Right(ScalarType("", "scala.Any"))
    else
      val segments = path.split("\\.").toList
      val (rootName, rootIndexes) = parseCurriedIndexes(segments.head)

      // 1) resolve the root
      val baseType: Either[String, FieldType] =
        schema match
          case c: ClassType =>
            // class root: look up field (with optional-unwrapping)
            resolveRootType(rootName, c)
          case ft =>
            // non-class root:
            // only valid if path is just that 1 segment
            if segments.tail.nonEmpty then
              Left(s"Cannot descend into non-class root type '${ft.typeName}'")
            else
              // return the schema as the root value
              Right(ft)

      // 2) apply indexes on the root (this is what was missing)
      val indexedRoot = baseType.flatMap { ft =>
        rootIndexes.foldLeft[Either[String, FieldType]](Right(ft)) { (acc, idx) =>
          acc.flatMap(t => unwrapContainer(t, idx))
        }
      }

      // 3) walk the rest (only if the overall schema is classy)
      indexedRoot.flatMap { rt =>
        if schema.isInstanceOf[ClassType] then
          walkSegments(rt, segments.tail)
        else
          // non-class root: if we reached here, either there were no tail
          // segments or we already errored above. So just return what we have.
          Right(rt)
      }