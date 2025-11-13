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

import zio.*

trait Statement:
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, DynaContext]

case class ValStmt[R](name: String, fn: Fn[R]) extends Statement:
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, DynaContext] =
    for {
      (value, lens) <- fn.resolve(ctx)
    } yield ctx.bind(name, value, lens)

case class MapStmt(path: String, fn: Fn[Any], posStr: String) extends Statement:
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, DynaContext] =
    ctx.get("top") match
      case Some((root, topLens)) =>
        ZIO.fail(DynaLensError(posStr, "Boom!  TODO--temporary error"))
      case None =>
        ZIO.fail(DynaLensError(posStr, "Missing 'top' in context for map statement"))


case class IfStmt(
    condition: Fn[Boolean],
    thenBlock: Statement,
    elseBlock: Option[Statement] = None
) extends Statement {

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, DynaContext] =
    for {
      (condAny, _) <- condition.resolve(ctx)
      cond = condAny.asInstanceOf[Boolean]
      resultCtx <-
        if cond then thenBlock.resolve(ctx)
        else elseBlock.map(_.resolve(ctx)).getOrElse(ZIO.succeed(ctx))
    } yield resultCtx
}


case class BlockStmt(statements: Seq[Statement]) extends Statement:
  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, DynaContext] =
    statements.foldLeft[ZIO[RuntimeEnv, DynaLensError, DynaContext]](ZIO.succeed(ctx)) { (accZio, stmt) =>
      accZio.flatMap { accCtx =>
        stmt.resolve(accCtx)
      }
    }


case class UpdateStmt[R](path: String, valueFn: Fn[R], posStr: String, updateFieldType: FieldType) extends Statement:
  // Coerce numeric RHS value to the expected field type (when both sides are numeric).
  private def coerceNumericIfNeeded(value: Any): Any =
    println(s"[coerceNumericIfNeeded] ENTER: value=${if value == null then "null" else value} (${if value == null then "null" else value.getClass.getName}) targetType=${updateFieldType.typeName}")
    try
      // Normalize type name and print debug info
      val rawType = updateFieldType.typeName
      val tn = rawType match {
        case "int" | "java.lang.Integer" => "scala.Int"
        case "long" | "java.lang.Long" => "scala.Long"
        case "float" | "java.lang.Float" => "scala.Float"
        case "double" | "java.lang.Double" => "scala.Double"
        case "byte" | "java.lang.Byte" => "scala.Byte"
        case "short" | "java.lang.Short" => "scala.Short"
        case "bigint" | "scala.math.BigInt" => "scala.math.BigInt"
        case "bigdecimal" | "scala.math.BigDecimal" => "scala.math.BigDecimal"
        case other => other
      }
      println(s"[coerceNumericIfNeeded] normalized typename raw='$rawType' normalized='$tn'")
      val coerced =
        if value != null && numericSet.contains(tn) then
          val result = util.NumPromote.toType(value, tn)
          println(s"[coerceNumericIfNeeded] Converted: ${value.getClass.getSimpleName} -> ${result.getClass.getSimpleName}  value=$value  result=$result")
          result
        else
          println(s"[coerceNumericIfNeeded] No conversion performed. valueClass=${if value == null then "null" else value.getClass.getName}")
          value
      println(s"[coerceNumericIfNeeded] EXIT returning ${if coerced == null then "null" else coerced.getClass.getName}")
      coerced
    catch
      case e: Throwable =>
        println(s"[coerceNumericIfNeeded] ERROR during coercion: ${e.getMessage}")
        value

  /** Walk the full path and return (parentObj, parentLens, lastElement)
   * where:
   *   - parentObj  = the object that owns the child we are updating
   *   - parentLens = the Lens for that parentObj
   *   - lastElement = the final PathElement (field or index) we want to update
   *
   * Handles optional parents — returns (None, lens, lastElement) for safe no-op.
   */
  private def walkToParent(
                            lens: Lens,
                            obj: Any,
                            path: List[PathElement]
                          ): ZIO[Any, DynaLensError, (Any, Lens, PathElement)] =

    // DEBUG: At the very start
    println(s"[walkToParent] ENTER lens=${lens.name}, objType=${if obj == null then "null" else obj.getClass.getName}, path=${path.map(_.name).mkString(".")}")

    val normalizedPath =
      path match
        case PathElement(Some("this"), None) :: rest => rest
        case other                                   => other

    normalizedPath match
      case Nil | _ :: Nil =>
        println(s"[walkToParent] ERROR: path too short for lens=${lens.name}")
        ZIO.dieMessage("walkToParent: must be called only when path has >=2 elements")

      case init :+ lastElem =>
        // Walk the prefix path to locate the parent object + lens
        def loop(
                  curLens: Lens,
                  curObj: Any,
                  rem: List[PathElement]
                ): ZIO[Any, DynaLensError, (Any, Lens)] = {
          // DEBUG: Entering loop in walkToParent
          println(s"[walkToParent.loop] lens=${curLens.name}, objType=${if curObj == null then "null" else curObj.getClass.getName}, rem=${rem.map(_.name).mkString(".")}")
          rem match
            case Nil =>
              println(s"[walkToParent.loop] SUCCESS returning lens=${curLens.name}")
              ZIO.succeed((curObj, curLens)) // done
            case PathElement(fieldNameOpt, indexOpt) :: tail =>
              curLens match
                case cl: ClassLens =>
                  curObj match
                    case None if cl.isOptional =>
                      println(s"[walkToParent.loop] SUCCESS returning lens=${cl.name}")
                      // Parent missing: safe no-op position reached, stop walking
                      ZIO.succeed((None, cl))
                    case _ =>
                      val baseObjZ =
                        if cl.isOptional then
                          curObj match
                            case Some(v) => ZIO.succeed(v)
                            case None => ZIO.succeed(None) // Optional parent missing
                            case other => ZIO.succeed(other)
                        else if curObj == null then
                          println(s"[walkToParent.loop] FAIL: Cannot descend into null for required class '${cl.name}'")
                          ZIO.fail(DynaLensError("", s"Cannot descend into null for required class '${cl.name}'"))
                        else ZIO.succeed(curObj)
                      for
                        baseObj <- baseObjZ
                        fieldName = fieldNameOpt.getOrElse("")
                        // Anchor handling for update traversal
                        _ <-
                          if fieldName == "this" then
                            // Stay at current scope
                            loop(curLens, curObj, tail).flatMap(res => ZIO.succeed(res)).asInstanceOf[ZIO[Any, DynaLensError, Unit]]
                          else if fieldName == "this_key" then
                            curObj match
                              case (k: Any, kvLens: Lens) =>
                                loop(ScalarLens("this_key", false, Some(curLens)), k, tail).flatMap(res => ZIO.succeed(res)).asInstanceOf[ZIO[Any, DynaLensError, Unit]]
                              case _ =>
                                println(s"[walkToParent.loop] FAIL: this_key used outside map key context")
                                ZIO.fail(DynaLensError("", "this_key used outside map key context")).asInstanceOf[ZIO[Any, DynaLensError, Unit]]
                          else if fieldName == "this_value" then
                            curObj match
                              case (_, v: Any) =>
                                loop(curLens, v, tail).flatMap(res => ZIO.succeed(res)).asInstanceOf[ZIO[Any, DynaLensError, Unit]]
                              case _ =>
                                println(s"[walkToParent.loop] FAIL: this_value used outside map value context")
                                ZIO.fail(DynaLensError("", "this_value used outside map value context")).asInstanceOf[ZIO[Any, DynaLensError, Unit]]
                          else ZIO.unit
                        fieldLens <- ZIO.fromOption(cl.fields.get(fieldName))
                          .orElse {
                            println(s"[walkToParent.loop] FAIL: No such field '$fieldName' in ${cl.name}")
                            ZIO.fail(DynaLensError("", s"No such field: $fieldName"))
                          }
                        fieldValue <- cl._get(fieldName, baseObj)
                        res <-
                          indexOpt match
                            case None =>
                              // No index/key on this hop: just descend into the child
                              loop(fieldLens, fieldValue, tail)
                            case Some(rawIdxOrKey) =>
                              fieldLens match
                                case ll: ListLens =>
                                  ReflectUtil.unwrapOptionList(fieldValue, ll.isOptional).flatMap { list =>
                                    val idx = rawIdxOrKey.toIntOption.getOrElse(-1)
                                    if idx < 0 || idx >= list.size then
                                      if ll.isOptional then {
                                        println(s"[walkToParent.loop] SUCCESS returning lens=${ll.name}")
                                        ZIO.succeed((None, ll)) // missing optional parent -> treat as None
                                      }
                                      else {
                                        println(s"[walkToParent.loop] FAIL: Index $idx out of bounds for ${ll.name}")
                                        ZIO.fail(DynaLensError("", s"Index $idx out of bounds for list '${ll.name}'"))
                                      }
                                    else
                                      loop(ll.elementLens, list(idx), tail)
                                  }
                                case ml: MapLens =>
                                  val typedKeyZ: ZIO[Any, DynaLensError, Any] = ml.keyKind match
                                    case MapKeyKind.StringKey       => ZIO.succeed(rawIdxOrKey)
                                    case MapKeyKind.IntKey          =>
                                      ZIO.fromOption(rawIdxOrKey.toIntOption)
                                        .orElse {
                                          println(s"[walkToParent.loop] FAIL: Invalid Int key '$rawIdxOrKey' for map '${ml.name}'")
                                          ZIO.fail(DynaLensError("", s"Invalid Int key '$rawIdxOrKey' for map '${ml.name}'"))
                                        }
                                    case MapKeyKind.LongKey         =>
                                      ZIO.attempt(rawIdxOrKey.toLong)
                                        .mapError { _ =>
                                          println(s"[walkToParent.loop] FAIL: Invalid Long key '$rawIdxOrKey' for map '${ml.name}'")
                                          DynaLensError("", s"Invalid Long key '$rawIdxOrKey' for map '${ml.name}'")
                                        }
                                    case MapKeyKind.EnumKey(eName)  =>
                                      ZIO.attempt(ReflectUtil.coerceEnumKey(rawIdxOrKey, eName))
                                        .mapError { e =>
                                          println(s"[walkToParent.loop] FAIL: Invalid enum key: ${e.getMessage}")
                                          DynaLensError("", s"Invalid enum key: ${e.getMessage}")
                                        }
                                  for
                                    typedKey <- typedKeyZ
                                    map     <- ReflectUtil.unwrapOptionMap[Any, Any](fieldValue, ml.isOptional)
                                    nextObj   = map.getOrElse(typedKey, None)
                                    nextLens  = ml.valueLens
                                    out <-
                                      if nextObj == None then
                                        if ml.isOptional then {
                                          println(s"[walkToParent.loop] SUCCESS returning lens=${ml.name}")
                                          ZIO.succeed((None, ml))
                                        }
                                        else {
                                          println(s"[walkToParent.loop] FAIL: Missing map key '$typedKey' for required map '${ml.name}'")
                                          ZIO.fail(DynaLensError("", s"Missing map key '$typedKey' for required map '${ml.name}'"))
                                        }
                                      else
                                        loop(nextLens, nextObj, tail)
                                  yield out
                                case _ =>
                                  println(s"[walkToParent.loop] FAIL: Index/key specified for non-collection field '$fieldName'")
                                  ZIO.fail(DynaLensError("", s"Index/key specified for non-collection field '$fieldName'"))
                      yield {
                        println(s"[walkToParent.loop] SUCCESS returning lens=${fieldLens.name}")
                        res
                      }

                case ll: ListLens =>
                  val idx = indexOpt.flatMap(_.toIntOption).getOrElse(-1)
                  ReflectUtil.unwrapOptionList(curObj, ll.isOptional).flatMap { list =>
                    if idx < 0 || idx >= list.size then
                      if ll.isOptional then {
                        println(s"[walkToParent.loop] SUCCESS returning lens=${ll.name}")
                        ZIO.succeed((None, ll))
                      }
                      else {
                        println(s"[walkToParent.loop] FAIL: Index $idx out of bounds for ${ll.name}")
                        ZIO.fail(DynaLensError("", s"Index $idx out of bounds for list '${ll.name}'"))
                      }
                    else
                      loop(ll.elementLens, list(idx), tail)
                  }
                case ml: MapLens =>
                  for
                    typedKey <- (ml.keyKind match
                      case MapKeyKind.StringKey => ZIO.succeed(indexOpt.get)
                      case MapKeyKind.IntKey =>
                        ZIO.fromOption(indexOpt.get.toIntOption)
                          .orElse {
                            println(s"[walkToParent.loop] FAIL: Invalid Int key '${indexOpt.get}' for map '${ml.name}'")
                            ZIO.fail(DynaLensError("", s"Invalid Int key '${indexOpt.get}' for map '${ml.name}'"))
                          }
                      case MapKeyKind.LongKey =>
                        ZIO.attempt(indexOpt.get.toLong)
                          .mapError { _ =>
                            println(s"[walkToParent.loop] FAIL: Invalid Long key '${indexOpt.get}' for map '${ml.name}'")
                            DynaLensError("", s"Invalid Long key '${indexOpt.get}' for map '${ml.name}'")
                          }
                      case MapKeyKind.EnumKey(enumName) =>
                        ZIO.attempt(ReflectUtil.coerceEnumKey(indexOpt.get, enumName))
                          .mapError { e =>
                            println(s"[walkToParent.loop] FAIL: Invalid enum key: ${e.getMessage}")
                            DynaLensError("", s"Invalid enum key: ${e.getMessage}")
                          }
                      )
                    map <- ReflectUtil.unwrapOptionMap[Any, Any](curObj, ml.isOptional)
                    nextObj = map.getOrElse(typedKey, None)
                    res <- loop(ml.valueLens, nextObj, tail)
                  yield {
                    println(s"[walkToParent.loop] SUCCESS returning lens=${ml.valueLens.name}")
                    res
                  }
                case sl: ScalarLens =>
                  println(s"[walkToParent.loop] FAIL: Cannot descend into scalar '${sl.name}'")
                  ZIO.fail(DynaLensError("", s"Cannot descend into scalar '${sl.name}'"))
                case el: EnumLens =>
                  println(s"[walkToParent.loop] FAIL: Cannot descend into enum '${el.name}'")
                  ZIO.fail(DynaLensError("", s"Cannot descend into enum '${el.name}'"))
        }
        loop(lens, obj, init).map { (parentObj, parentLens) =>
          println(s"[walkToParent] EXIT parentLens=${parentLens.name}, lastElem=${lastElem.name.getOrElse("<none>")}")
          (parentObj, parentLens, lastElem)
        }

      case _ =>
        // This case is unreachable under normal circumstances, but makes the match exhaustive
        ZIO.fail(DynaLensError("", s"Unexpected path structure: ${normalizedPath.mkString("/")}"))

  // Helper to perform update with error handling and coercion
  private def performUpdate(
      rootLens: Lens,
      rootObj: Any,
      elements: List[PathElement],
      newValue: Any,
      posStr: String,
      path: String
  ): ZIO[Any, DynaLensError, Any] =
    val coerced = coerceNumericIfNeeded(newValue)
    println(s"[performUpdate] coercing ${newValue} -> ${if coerced == null then "null" else coerced.getClass.getName}")
    // --- DEBUG instrumentation before rootLens.update
    println(s"[performUpdate] rootObj class: ${if rootObj == null then "null" else rootObj.getClass.getName}")
    println(s"[performUpdate] elements: ${elements.map(_.name.getOrElse("<none>")).mkString(",")}")
    println(s"[performUpdate] lens: ${rootLens.name}")
    // Guard: If rootObj is a Map, warn and extract value from "top" key if it exists, else fail
    val safeRootObjZIO: ZIO[Any, DynaLensError, Any] =
      rootObj match {
        case m: scala.collection.Map[?, ?] =>
          println(s"[performUpdate] WARNING: rootObj is a Map (${m.getClass.getName}), attempting to extract value from 'top' key")
          m.asInstanceOf[scala.collection.Map[Any, Any]].get("top") match {
            case Some(actualValue) =>
              println(s"[performUpdate] Extracted value for 'top' key: class=${if actualValue == null then "null" else actualValue.getClass.getName}")
              ZIO.succeed(actualValue)
            case None =>
              println(s"[performUpdate] ERROR: Map rootObj does not contain 'top' key")
              ZIO.fail(DynaLensError(posStr, s"rootObj is a Map but does not contain 'top' key (likely context error, e.g. Map$$Map1 → Person class cast issue)"))
          }
        case _ =>
          ZIO.succeed(rootObj)
      }
    safeRootObjZIO.flatMap { safeRootObj =>
      // Unwrap (value, lens) tuple if present
      val realRootObj = safeRootObj match {
        case (v, _: Lens) =>
          println(s"[performUpdate] Unwrapped (value,lens) tuple, using ${if v == null then "null" else v.getClass.getName}")
          v
        case other =>
          other
      }
      println(s"[performUpdate] realRootObj class: ${if realRootObj == null then "null" else realRootObj.getClass.getName}")
      ZIO
        .attempt(rootLens.update(
          elements.filterNot(pe =>
            pe.name.contains("this") ||
            pe.name.contains("this_key") ||
            pe.name.contains("this_value")
          ),
          coerced,
          realRootObj
        ))
        .flatten
        .mapError {
          case _: ClassCastException =>
            DynaLensError(
              posStr,
              s"Type mismatch: cannot assign value of type ${newValue.getClass.getName} to path '$path'"
            )
          case e =>
            DynaLensError(posStr, s"Unexpected update error: ${e.getMessage}")
        }
    }

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, DynaContext] =
    val rootObj = ctx.rootObj
    val rootLens = ctx.rootLens
    if rootObj == null then
      ZIO.fail(DynaLensError(posStr, "Receiver object is null — cannot update"))
    else
      val elements0 = Path.parsePath(path)
      val (startsWithThis, elements) = elements0 match
        case PathElement(Some("this"), None) :: rest => (true, rest)
        case _ => (false, elements0)
      for
        ctxWithThis <- ZIO.succeed(ctx.bind("this", rootObj, rootLens))
        updatedObj <-
          if elements.isEmpty then
            ZIO.fail(DynaLensError(posStr, "Assignment to `this` is not allowed"))
          else if !startsWithThis then
            if elements.lengthCompare(1) > 0 then
              val unwrappedRootObj = rootObj match {
                case (v, _: Lens) =>
                  println(s"[resolve] Unwrapped (value,lens) tuple before walkToParent → ${v.getClass.getName}")
                  v
                case other => other
              }
              walkToParent(rootLens, unwrappedRootObj, elements).flatMap {
                case (None, parentLens, lastElem) if parentLens.isOptional =>
                  println(s"[updateStmt] Early return: optional parent missing for path=$path")
                  ZIO.succeed(unwrappedRootObj)
                case (parentObj0, parentLens0, lastElem0) =>
                  println(s"[updateStmt] Proceeding with update for path=$path")
                  val (parent, parentLens, lastElem) = (parentObj0, parentLens0, lastElem0)
                  val ctxForRhs = ctx.bind("this", parent, parentLens)
                  // --- DEBUG: Before valueFn.resolve
                  println(s"[UpdateStmt] About to resolve valueFn for path='$path'")
                  println(s"[UpdateStmt] ctxForRhs.this-binding: ${ctxForRhs.get("this")}")
                  println(s"[UpdateStmt] valueFn: $valueFn")
                  println(s"[UpdateStmt] Using ctxForRhs.this-binding: ${ctxForRhs.get("this")}")
                  for {
                    res <-
                      valueFn.resolve(ctxForRhs)
                        .catchAll { e1 =>
                          println(s"[UpdateStmt] Fallback ctxWithThis.this-binding: ${ctxWithThis.get("this")}")
                          println(s"[UpdateStmt] valueFn.resolve(ctxForRhs) failed: $e1, trying ctxWithThis")
                          valueFn.resolve(ctxWithThis).catchAll { e2 =>
                            println(s"[UpdateStmt] valueFn.resolve(ctxWithThis) also failed: $e2")
                            ZIO.fail(e1)
                          }
                        }
                    (newValue, _) = res
                    updatedObj <- {
                      // --- DEBUG: After valueFn.resolve
                      println(s"[UpdateStmt] valueFn.resolve result for path='$path': newValue=${newValue} (${if newValue == null then "null" else newValue.getClass.getName})")
                      newValue match
                        case c: Iterable[?] =>
                          println(s"[UpdateStmt] newValue is Iterable, size=${c.size}, type=${c.getClass.getName}")
                        case arr: Array[?] =>
                          println(s"[UpdateStmt] newValue is Array, length=${arr.length}, type=${arr.getClass.getName}")
                        case other =>
                          println(s"[UpdateStmt] newValue is type=${if other == null then "null" else other.getClass.getName}")
                      // --- DEBUG: Before performUpdate
                      println(s"[UpdateStmt] About to call performUpdate for path='$path' with newValue=${newValue}")
                      performUpdate(rootLens, rootObj, elements, newValue, posStr, path)
                    }
                  } yield updatedObj
                case null =>
                  println(s"[updateStmt] walkToParent returned null for path=$path")
                  ZIO.dieMessage("walkToParent returned unexpected tuple")
              }
            else
              rootObj match
                case None if rootLens.isOptional =>
                  ZIO.succeed(rootObj)
                case None =>
                  ZIO.fail(DynaLensError(posStr, s"Cannot update '$path': parent is missing"))
                case _ =>
                  for {
                    res <- valueFn.resolve(ctxWithThis)
                    (newValue, _) = res
                    updatedObj <- performUpdate(rootLens, rootObj, elements, newValue, posStr, path)
                  } yield updatedObj
          else
            elements match
              case lastElem :: Nil =>
                rootObj match
                  case None =>
                    ZIO.succeed(rootObj)
                  case _ =>
                    for {
                      res <- valueFn.resolve(ctxWithThis)
                      (newValue, _) = res
                      updatedObj <- performUpdate(rootLens, rootObj, List(lastElem), newValue, posStr, path)
                    } yield updatedObj
              case _ =>
                walkToParent(rootLens, rootObj, elements).flatMap {
                  case (None, parentLens, lastElem) =>
                    ZIO.succeed(rootObj)
                  case (Some(parent), parentLens, lastElem) =>
                    val ctxForRhs = ctx.bind("this", parent, parentLens)
                    for {
                      res <- valueFn.resolve(ctxForRhs).catchAll { e1 =>
                        valueFn.resolve(ctxWithThis).catchAll { _ => ZIO.fail(e1) }
                      }
                      (newValue, _) = res
                      updatedObj <- performUpdate(rootLens, rootObj, elements, newValue, posStr, path)
                    } yield updatedObj
                  case _ =>
                    ZIO.dieMessage("walkToParent returned unexpected tuple")
                }
        updatedCtx = ctx.copy(
          rootObj = updatedObj,
          symbols = ctx.symbols + ("this" -> (updatedObj, ctx.rootLens))
        )
      yield updatedCtx.unbind("this")