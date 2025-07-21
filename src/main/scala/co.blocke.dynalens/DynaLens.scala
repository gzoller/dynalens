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
import scala.quoted.*
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.reflect.rtypeRefs.{FieldInfoRef, ScalaClassRef, SeqRef}
import co.blocke.scala_reflection.TypedName
import scala.collection.mutable

import Path.*

case class DynaLensError(msg: String)

case class DynaLens[T](
    _update: (String, Any, T) => ZIO[Any, DynaLensError, T],
    _get: (String, T) => ZIO[Any, DynaLensError, Any],
    _registry: Map[String, DynaLens[?]],
    _typeName: String
):
  type ThisT = T

  // Run a compiled lens script
  inline def run(
      script: BlockStmt,
      target: T,
      registry: _BiMapRegistry = EmptyBiMapRegistry
  ): ZIO[Any, DynaLensError, (T, Map[String, (Any, DynaLens[?])])] =
    actualRun(script, target).provide(BiMapRegistry.layer(registry))

  private inline def actualRun(
      script: BlockStmt,
      target: T
  ): ZIO[_BiMapRegistry, DynaLensError, (T, Map[String, (Any, DynaLens[?])])] =
    val ctx = Map("top" -> (target, this))
    for {
      resultCtx <- script.resolve(ctx)
      (resultObj, _) = resultCtx("top")
    } yield (resultObj.asInstanceOf[T], resultCtx)

  def runNoZIO(script: BlockStmt, target: T, registry: _BiMapRegistry = EmptyBiMapRegistry): Either[DynaLensError, (T, Map[String, (Any, DynaLens[?])])] =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe
        .run(
          run(script, target, registry).either
        )
        .getOrThrow()
    }

  def get(path: String, obj: T): ZIO[Any, DynaLensError, Any] = {
    val parsed = parsePath(path)

    def step(current: Any, currentLens: DynaLens[?], path: List[PathElement]): ZIO[Any, DynaLensError, Any] =
      path match {
        case Nil =>
          ZIO.succeed(current)

        case Field(f) :: rest =>
          currentLens
            ._get(f, current.asInstanceOf[currentLens.ThisT])
            .flatMap { value =>
              currentLens._registry.get(f) match {
                case Some(nextLens) =>
                  step(value, nextLens, rest)
                case None =>
                  if rest.isEmpty then ZIO.succeed(value)
                  else step(value, currentLens, rest)
              }
            }

        case IndexedField(f, i) :: rest =>
          currentLens
            ._get(f, current.asInstanceOf[currentLens.ThisT])
            .flatMap {
              case list: Seq[Any] =>
                if i < 0 && rest == Nil then // return whole Seq
                  ZIO.succeed(list)
                else
                  list.lift(i) match {
                    case Some(elem) =>
                      currentLens._registry.get(f) match {
                        case Some(elemLens) =>
                          step(elem, elemLens, rest)
                        case None =>
                          if rest.isEmpty then ZIO.succeed(elem)
                          else ZIO.fail(DynaLensError(s"No registry for '$f' to recurse into index"))
                      }
                    case None =>
                      ZIO.fail(DynaLensError(s"Index $i out of bounds"))
                  }

              case other =>
                ZIO.fail(DynaLensError(s"Expected sequence at '$f', but got ${other.getClass.getSimpleName}"))
            }
      }

    step(obj, this, parsed)
  }

  def update(path: String, value: Any, obj: T): ZIO[Any, DynaLensError, T] = {
    val parsed = parsePath(path)

    def step(current: Any, currentLens: DynaLens[?], path: List[PathElement]): ZIO[Any, DynaLensError, Any] = path match {
      case Nil =>
        ZIO.fail(DynaLensError("Cannot update empty path"))

      case Field(f) :: Nil =>
        currentLens
          ._update(f, value, current.asInstanceOf[currentLens.ThisT])

      case Field(f) :: rest =>
        for {
          nested <- currentLens._get(f, current.asInstanceOf[currentLens.ThisT])
          nextLens <- currentLens._registry.get(f) match
            case Some(a) => ZIO.succeed(a)
            case None    => ZIO.fail(DynaLensError(s"No nested lens for field '$f'"))
          updatedNested <- step(nested, nextLens, rest)
          updated <- currentLens._update(f, updatedNested, current.asInstanceOf[currentLens.ThisT])
        } yield updated

      case IndexedField(f, i) :: rest =>
        for {
          rawList <- currentLens._get(f, current.asInstanceOf[currentLens.ThisT])
          list <- ZIO
            .attempt(rawList.asInstanceOf[Seq[Any]])
            .mapError(_ => DynaLensError(s"Field '$f' is not a Seq"))
          elem <- list.lift(i) match
            case Some(e) => ZIO.succeed(e)
            case None    => ZIO.fail(DynaLensError(s"Index $i out of bounds for field '$f'"))
          nextLens <- currentLens._registry.get(f) match
            case Some(a) => ZIO.succeed(a)
            case None    => ZIO.fail(DynaLensError(s"No nested lens for collection field '$f'"))
          updatedElem <- step(elem, nextLens, rest)
          updatedList = list.updated(i, updatedElem)
          updated <- currentLens._update(f, updatedList, current.asInstanceOf[currentLens.ThisT])
        } yield updated
    }

    step(obj, this, parsed).asInstanceOf[ZIO[Any, DynaLensError, T]]
  }

  private def walkPath(
      path: List[PathElement],
      current: Any,
      dynalens: DynaLens[?]
  ): ZIO[Any, DynaLensError, mutable.Map[String, (Any, DynaLens[?])]] = {

    val ctx = mutable.Map[String, (Any, DynaLens[?])]("top" -> (current, dynalens))

    def step(
        path: List[PathElement],
        currentLens: DynaLens[?]
    ): ZIO[Any, DynaLensError, Unit] = path match {
      case Field(f) :: rest =>
        currentLens._registry.get(f) match
          case Some(nextLens) => step(rest, nextLens)
          case None           => step(rest, currentLens)

      case IndexedField(f, _) :: rest =>
        currentLens._registry.get(f) match {
          case Some(loopLens) =>
            // Insert into top-level map
            ctx += (f -> (null, loopLens))
            step(rest, loopLens)
          case None =>
            ZIO.fail(DynaLensError(s"No lens found for loop field '$f'"))
        }

      case Nil =>
        ZIO.unit
    }

    step(path, dynalens).as(ctx)
  }

  // Split path at Iterables to create sub-paths
  private def splitIntoLevels(path: List[PathElement]): List[List[PathElement]] = {
    val (levels, current) = path.foldLeft(List.empty[List[PathElement]] -> List.empty[PathElement]) {
      case ((acc, current), pe @ IndexedField(_, -1)) =>
        (acc :+ (current :+ pe)) -> Nil
      case ((acc, current), pe) =>
        acc -> (current :+ pe)
    }

    (levels :+ current).filter(_.nonEmpty)
  }

  def map[R](
      path: String,
      fn: Fn[R],
      obj: T,
      outerCtx: Map[String, (Any, DynaLens[?])] = Map.empty // <-- added outer context
  ): ZIO[_BiMapRegistry, DynaLensError, T] =

    def processPaths(
        paths: List[List[PathElement]],
        refObj: Any,
        dynalens: DynaLens[?],
        ctx: mutable.Map[String, (Any, DynaLens[?])]
    ): ZIO[_BiMapRegistry, DynaLensError, Any] =
      paths match {
        case pathParts :: Nil =>
          val partialPath = Path.partialPath(pathParts)
          for {
            in <- dynalens.get(partialPath, refObj.asInstanceOf[dynalens.ThisT])
            maybeLens = pathParts.last match {
              case IndexedField(p, _) => dynalens._registry.get(p).getOrElse(null)
              case Field(p)           => null
            }
            _ = ctx.put("this", (in, maybeLens)) // assign loop param variable
            enrichedCtx = outerCtx ++ ctx.toMap // <-- merge loop context with outer context
            out <- fn.resolve(enrichedCtx)
//              .mapError{ e => DynaLensError(s"Map function input value of $in is of the wrong data type.")}
            updated <- dynalens.update(partialPath, out, refObj.asInstanceOf[dynalens.ThisT])
          } yield updated

        case pathParts :: rest =>
          val partialPath = Path.partialPath(pathParts)
          val loopKey = pathParts.last.name
          for {
            listVal <- dynalens.get(partialPath, refObj.asInstanceOf[dynalens.ThisT])
            iterable <- ZIO.fromEither(listVal match {
              case i: Iterable[?] => Right(i)
              case other =>
                Left(DynaLensError(s"Expected iterable at path '$partialPath', but found: ${other.getClass.getName}"))
            })
            lensForList <- ctx.get(loopKey) match {
              case Some((_, existingLens)) => ZIO.succeed(existingLens)
              case None =>
                ZIO.fail(DynaLensError(s"No existing lens found in ctx for key: $loopKey"))
            }
            updatedIterable <- ZIO.foreach(iterable) { item =>
              ctx.update(loopKey, (item, lensForList)) // update ctx with current item
              processPaths(rest, item, lensForList, ctx) // recurse
            }
            updatedRefObj <- dynalens.update(partialPath, updatedIterable, refObj.asInstanceOf[dynalens.ThisT])
          } yield updatedRefObj

        case Nil =>
          ZIO.fail(DynaLensError("Should Never Happen(tm)"))
      }

    val parsed = parsePath(path)
    for {
      ctx <- walkPath(parsed, obj, this)
      splitPaths = splitIntoLevels(parsed)
      updated <- processPaths(splitPaths, obj, this, ctx)
    } yield updated.asInstanceOf[T]

object DynaLens:

  inline def dynalens[T]: DynaLens[T] = ${ generateDynaLensImpl[T] }

  private def generateDynaLensImpl[T: Type](using Quotes): Expr[DynaLens[T]] =
    import quotes.reflect.*

    ReflectOnType[T](quotes)(TypeRepr.of[T])(using scala.collection.mutable.Map.empty[TypedName, Boolean]) match {
      case s: ScalaClassRef[T] if s.isCaseClass =>
        val getLambdaExpr = generateGetLambda[T](quotes, s.fields)
        val updateLambdaExpr = generateUpdateLambda[T](quotes, s.fields)

        // Build recursive registry of DynaLens for any fields of this class which are also case classes
        val pairs: List[Expr[(String, DynaLens[?])]] = s.fields.flatMap { field =>
          field.fieldRef match {
            case c: ScalaClassRef[?] if c.isCaseClass =>
              c.refType match
                case '[t] =>
                  val lensExpr = generateDynaLensImpl[t] // recursive call
                  val keyExpr = Expr(field.name)
                  Some('{ $keyExpr -> $lensExpr })
            case c: SeqRef[?] =>
              c.elementRef match
                case d: ScalaClassRef[?] if d.isCaseClass =>
                  d.refType match
                    case '[t] =>
                      val lensExpr = generateDynaLensImpl[t]
                      val keyExpr = Expr(field.name)
                      Some('{ $keyExpr -> $lensExpr })
                case _ => None
            case _ => None
          }
        }
        val registryExpr: Expr[Map[String, DynaLens[?]]] = {
          val listExpr: Expr[List[(String, DynaLens[?])]] = Expr.ofList(pairs)
          '{ Map.from[String, DynaLens[?]]($listExpr) }
        }
        val typeNameExpr = Expr(s.typedName.toString)

        '{ DynaLens[T]($updateLambdaExpr, $getLambdaExpr, $registryExpr, $typeNameExpr) }

      case x => throw new Exception(s"Sorry, dynalens only supports Scala case classes but received ${x.name}")
    }

  private def generateGetLambda[T: Type](quotes: Quotes, classFields: List[FieldInfoRef]): Expr[(String, T) => ZIO[Any, DynaLensError, Any]] =
    import quotes.reflect.*
    given Quotes = quotes

    val tpe = TypeRepr.of[T]

    Lambda(
      Symbol.spliceOwner,
      MethodType(List("field", "target"))(
        _ => List(TypeRepr.of[String], tpe),
        _ => TypeRepr.of[ZIO[Any, DynaLensError, Any]]
      ),
      (_, params) => {
        val fieldParam = params(0).asInstanceOf[Term]
        val targetParam = params(1).asInstanceOf[Term]

        val matchExpr = Match(
          fieldParam,
          classFields.map { f =>
            val fieldName = f.name
            CaseDef(
              Literal(StringConstant(fieldName)),
              None,
              '{ ZIO.succeed(${ Select.unique(targetParam, fieldName).asExpr }) }.asTerm
            )
          } :+ CaseDef(
            Wildcard(),
            None,
            '{ ZIO.fail(DynaLensError("Field not found: " + ${ fieldParam.asExprOf[String] })) }.asTerm
          )
        )

        matchExpr
      }
    ).asExprOf[(String, T) => ZIO[Any, DynaLensError, Any]]

  private def generateUpdateLambda[T: Type](quotes: Quotes, classFields: List[FieldInfoRef]): Expr[(String, Any, T) => ZIO[Any, DynaLensError, T]] =
    import quotes.reflect.*
    given Quotes = quotes

    val tpe = TypeRepr.of[T]
    val sym = tpe.typeSymbol
    val fields = sym.caseFields

    // Define parameter symbols: (fieldName: String, value: Any, target: T)
    val methodSym = Symbol.spliceOwner
    val paramNames = List("field", "value", "target")
    val paramTypes = List(TypeRepr.of[String], TypeRepr.of[Any], tpe)

    val methodType = MethodType(paramNames)(_ => paramTypes, _ => TypeRepr.of[ZIO[Any, DynaLensError, T]])

    Lambda(
      methodSym,
      methodType,
      (_, params) => {
        val fieldParam = params(0).asInstanceOf[Term] // field: String
        val valueParam = params(1).asInstanceOf[Term] // value: Any
        val targetParam = params(2).asInstanceOf[Term] // target: T

        // Build cases
        val cases = classFields.map { field =>
          val name = field.name
          val fieldType = field.fieldRef.refType
          val copyArgs = fields.map { f =>
            if f.name == name then NamedArg(f.name, TypeApply(Select.unique(valueParam, "asInstanceOf"), List(TypeTree.of(using fieldType))).asExpr.asTerm)
            else NamedArg(f.name, Select.unique(targetParam, f.name))
          }

          val updatedExpr = Apply(Select.unique(targetParam, "copy"), copyArgs)

          CaseDef(Literal(StringConstant(name)), None, '{ ZIO.succeed(${ updatedExpr.asExprOf[T] }) }.asTerm)
        }

        val fallback = CaseDef(Wildcard(), None, '{ ZIO.fail(DynaLensError("Field not found: " + ${ fieldParam.asExprOf[String] })) }.asTerm)

        Match(fieldParam, cases :+ fallback)
      }
    ).asExprOf[(String, Any, T) => ZIO[Any, DynaLensError, T]]
