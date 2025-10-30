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


case class UpdateStmt[R](path: String, valueFn: Fn[R], posStr: String) extends Statement:

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

    val normalizedPath =
      path match
        case PathElement(Some("this"), None) :: rest => rest
        case other                                   => other

    normalizedPath match
      case Nil | _ :: Nil =>
        ZIO.dieMessage("walkToParent: must be called only when path has >=2 elements")

      case init :+ lastElem =>
        // Walk the prefix path to locate the parent object + lens
        def loop(
                  curLens: Lens,
                  curObj: Any,
                  rem: List[PathElement]
                ): ZIO[Any, DynaLensError, (Any, Lens)] = {
          // DEBUG: Entering loop in walkToParent
          rem match
            case Nil =>
              ZIO.succeed((curObj, curLens)) // done
            case PathElement(fieldNameOpt, indexOpt) :: tail =>
              curLens match
                case cl: ClassLens =>
                  curObj match
                    case None if cl.isOptional =>
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
                                // Treat key as scalar lens; remain at same lens but change object to key
                                loop(ScalarLens("this_key", false, Some(curLens)), k, tail).flatMap(res => ZIO.succeed(res)).asInstanceOf[ZIO[Any, DynaLensError, Unit]]
                              case _ =>
                                ZIO.fail(DynaLensError("", "this_key used outside map key context")).asInstanceOf[ZIO[Any, DynaLensError, Unit]]
                          else if fieldName == "this_value" then
                            curObj match
                              case (_, v: Any) =>
                                // Treat value as object itself; keep current lens for traversal
                                loop(curLens, v, tail).flatMap(res => ZIO.succeed(res)).asInstanceOf[ZIO[Any, DynaLensError, Unit]]
                              case _ =>
                                ZIO.fail(DynaLensError("", "this_value used outside map value context")).asInstanceOf[ZIO[Any, DynaLensError, Unit]]
                          else ZIO.unit
                        fieldLens <- ZIO.fromOption(cl.fields.get(fieldName))
                          .orElseFail(DynaLensError("", s"No such field: $fieldName"))
                        fieldValue <- cl._get(fieldName, baseObj)
                        res <-
                          indexOpt match
                            case None =>
                              // No index/key on this hop: just descend into the child
                              loop(fieldLens, fieldValue, tail)
                            case Some(rawIdxOrKey) =>
                              fieldLens match
                                case ll: ListLens =>
                                  // items[<idx>] — unwrap, pick element, continue with element lens
                                  ReflectUtil.unwrapOptionList(fieldValue, ll.isOptional).flatMap { list =>
                                    val idx = rawIdxOrKey.toIntOption.getOrElse(-1)
                                    if idx < 0 || idx >= list.size then
                                      if ll.isOptional then ZIO.succeed((None, ll)) // missing optional parent -> treat as None
                                      else ZIO.fail(DynaLensError("", s"Index $idx out of bounds for list '${ll.name}'"))
                                    else
                                      val elemObj  = list(idx)
                                      val elemLens = ll.elementLens
                                      loop(elemLens, elemObj, tail)
                                  }
                                case ml: MapLens =>
                                  // things["key"] — coerce key, fetch entry (or None), continue with value lens
                                  val typedKeyZ: ZIO[Any, DynaLensError, Any] = ml.keyKind match
                                    case MapKeyKind.StringKey       => ZIO.succeed(rawIdxOrKey)
                                    case MapKeyKind.IntKey          => ZIO.fromOption(rawIdxOrKey.toIntOption)
                                      .orElseFail(DynaLensError("", s"Invalid Int key '$rawIdxOrKey' for map '${ml.name}'"))
                                    case MapKeyKind.LongKey         => ZIO.attempt(rawIdxOrKey.toLong)
                                      .mapError(_ => DynaLensError("", s"Invalid Long key '$rawIdxOrKey' for map '${ml.name}'"))
                                    case MapKeyKind.EnumKey(eName)  => ZIO.attempt(ReflectUtil.coerceEnumKey(rawIdxOrKey, eName))
                                      .mapError(e => DynaLensError("", s"Invalid enum key: ${e.getMessage}"))
                                  for
                                    typedKey <- typedKeyZ
                                    map     <- ReflectUtil.unwrapOptionMap[Any, Any](fieldValue, ml.isOptional)
                                    nextObj   = map.getOrElse(typedKey, None)
                                    nextLens  = ml.valueLens
                                    // Inserted type check for missing optional key
                                    out <-
                                      if nextObj == None then
                                        if ml.isOptional then
                                        // optional parent missing → no-op
                                          ZIO.succeed((None, ml))
                                        else
                                          // required -> proper error
                                          ZIO.fail(DynaLensError("", s"Missing map key '$typedKey' for required map '${ml.name}'"))
                                        else
                                          loop(nextLens, nextObj, tail)
                                  yield out
                                case _ =>
                                  // Index/key provided for a non-collection field
                                  ZIO.fail(DynaLensError("", s"Index/key specified for non-collection field '$fieldName'"))
                      yield res

                case ll: ListLens =>
                  val idx = indexOpt.flatMap(_.toIntOption).getOrElse(-1)
                  ReflectUtil.unwrapOptionList(curObj, ll.isOptional).flatMap { list =>
                    if idx < 0 || idx >= list.size then
                      if ll.isOptional then ZIO.succeed((None, ll))
                      else ZIO.fail(DynaLensError("", s"Index $idx out of bounds for list '${ll.name}'"))
                    else
                      loop(ll.elementLens, list(idx), tail)
                  }
                case ml: MapLens =>
                  for
                    typedKey <- (ml.keyKind match
                      case MapKeyKind.StringKey => ZIO.succeed(indexOpt.get)
                      case MapKeyKind.IntKey =>
                        ZIO.fromOption(indexOpt.get.toIntOption)
                          .orElseFail(DynaLensError("", s"Invalid Int key '${indexOpt.get}' for map '${ml.name}'"))
                      case MapKeyKind.LongKey =>
                        ZIO.attempt(indexOpt.get.toLong)
                          .mapError(_ => DynaLensError("", s"Invalid Long key '${indexOpt.get}' for map '${ml.name}'"))
                      case MapKeyKind.EnumKey(enumName) =>
                        ZIO.attempt(ReflectUtil.coerceEnumKey(indexOpt.get, enumName))
                          .mapError(e => DynaLensError("", s"Invalid enum key: ${e.getMessage}"))
                      )
                    map <- ReflectUtil.unwrapOptionMap[Any, Any](curObj, ml.isOptional)
                    nextObj = map.getOrElse(typedKey, None)
                    res <- loop(ml.valueLens, nextObj, tail)
                  yield res
                case sl: ScalarLens =>
                  ZIO.fail(DynaLensError("", s"Cannot descend into scalar '${sl.name}'"))
                case el: EnumLens =>
                  ZIO.fail(DynaLensError("", s"Cannot descend into enum '${el.name}'"))
        }
        loop(lens, obj, init).map { (parentObj, parentLens) =>
          (parentObj, parentLens, lastElem)
        }

      case _ =>
        // This case is unreachable under normal circumstances, but makes the match exhaustive
        ZIO.fail(DynaLensError("", s"Unexpected path structure: ${normalizedPath.mkString("/")}"))  

  def resolve(ctx: DynaContext): ZIO[RuntimeEnv, DynaLensError, DynaContext] =
    ctx.get("top") match
      case None =>
        ZIO.fail(DynaLensError(posStr, "Missing 'top' in context for update statement"))
      case Some((rootObj, rootLens)) =>
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
                  walkToParent(rootLens, rootObj, elements).flatMap {
                    case (None, parentLens, lastElem) if parentLens.isOptional =>
                      ZIO.succeed(rootObj)
                    case (parentObj0, parentLens0, lastElem0) =>
                      val (parent, parentLens, lastElem) = (parentObj0, parentLens0, lastElem0)
                      val ctxForRhs = ctx.bind("this", parent, parentLens)
                      for
                        (newValue, _) <-
                          valueFn.resolve(ctxForRhs).catchAll { e1 =>
                            // Fallback to resolving from root context when missing field on element-this
                            valueFn.resolve(ctxWithThis).catchAll { _ =>
                              ZIO.fail(e1) // original error if root also fails
                            }
                          }
                        // Important: perform the full update through the root lens so child update bubbles back into containers
                        updated <- {
                          val cleaned = elements.filterNot(pe =>
                            pe.name.contains("this") ||
                              pe.name.contains("this_key") ||
                              pe.name.contains("this_value")
                          )
                          ZIO
                            .attempt(rootLens.update(cleaned, newValue, rootObj))
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
                      yield updated
                    case null =>
                      ZIO.dieMessage("walkToParent returned unexpected tuple")
                  }
                else
                  rootObj match
                    case None if rootLens.isOptional =>
                      ZIO.succeed(rootObj)
                    case None =>
                      ZIO.fail(DynaLensError(posStr, s"Cannot update '$path': parent is missing"))
                    case _ =>
                      for
                        (newValue, _) <- valueFn.resolve(ctxWithThis)
                        updated <- {
                          val cleaned = elements.filterNot(pe =>
                            pe.name.contains("this") ||
                              pe.name.contains("this_key") ||
                              pe.name.contains("this_value")
                          )
                          ZIO
                            .attempt(rootLens.update(cleaned, newValue, rootObj))
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
                      yield updated
              else
                elements match
                  case lastElem :: Nil =>
                    rootObj match
                      case None =>
                        ZIO.succeed(rootObj)
                      case _ =>
                        for
                          (newValue, _) <- valueFn.resolve(ctxWithThis)
                          updated <- ZIO
                            .attempt(rootLens.update(List(lastElem), newValue, rootObj))
                            .flatten
                            .mapError {
                              case _: ClassCastException =>
                                DynaLensError(posStr,
                                  s"Type mismatch: cannot assign value of type ${newValue.getClass.getName} to path '$path'"
                                )
                              case e =>
                                DynaLensError(posStr, s"Unexpected update error: ${e.getMessage}")
                            }
                        yield updated
                  case _ =>
                    walkToParent(rootLens, rootObj, elements).flatMap {
                      case (None, parentLens, lastElem) =>
                        ZIO.succeed(rootObj)
                      case (Some(parent), parentLens, lastElem) =>
                        val ctxForRhs = ctx.bind("this", parent, parentLens)
                        for
                          (newValue, _) <-
                            valueFn.resolve(ctxForRhs).catchAll { e1 =>
                              // Fallback to resolving from root context when missing field on element-this
                              valueFn.resolve(ctxWithThis).catchAll { _ =>
                                ZIO.fail(e1) // original error if root also fails
                              }
                            }
                          // Important: perform the full update through the root so the mutation bubbles up the full object graph
                          updated <- {
                            val cleaned = elements.filterNot(pe =>
                              pe.name.contains("this") ||
                                pe.name.contains("this_key") ||
                                pe.name.contains("this_value")
                            )
                            ZIO
                              .attempt(rootLens.update(cleaned, newValue, rootObj))
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
                        yield updated
                      case _ =>
                        ZIO.dieMessage("walkToParent returned unexpected tuple")
                    }
            updatedCtx = ctx.bind("top", updatedObj, rootLens)
          yield updatedCtx.unbind("this")