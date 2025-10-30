package co.blocke.dynalens

import zio.*
import scala.quoted.*
import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.reflect.*
import co.blocke.scala_reflection.reflect.rtypeRefs.*



final case class DynaLens[T](
                           topLens: ClassLens,
                           schema: ClassType,
                           registry: Map[String, Lens] = Map.empty
                         ):
  // Run a compiled lens script
  inline def run(
                  script: BlockStmt,
                  target: T,
                  registry: _BiMapRegistry = EmptyBiMapRegistry
                ): ZIO[Any, DynaLensError, (T, DynaContext)] =
    actualRun(script, target).provide(
      ZLayer.succeed(RuntimeEnv(registry))
    )

  private inline def actualRun(
                                script: BlockStmt,
                                target: T
                              ): ZIO[RuntimeEnv, DynaLensError, (T, DynaContext)] =
    val ctx: DynaContext = this.initialContext(target)
    for {
      resultCtx <- script.resolve(ctx)
      (resultObj, _) = resultCtx.getTop.getOrElse((target, topLens))
    } yield (resultObj.asInstanceOf[T], resultCtx)

  def runNoZIO(script: BlockStmt, target: T, registry: _BiMapRegistry = EmptyBiMapRegistry): Either[DynaLensError, (T, DynaContext)] =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe
        .run(
          run(script, target, registry).either
        )
        .getOrThrow()
    }


extension [T](dl: DynaLens[T])
  /** Creates an initial context with the top-level value bound to 'this', 'this_value', and 'this_key'. */
  def initialContext(topValue: Any): DynaContext =
    val base = DynaContext(Map.empty, dl)
      .bind("this", topValue, dl.topLens)
      .bind("this_value", topValue, dl.topLens)
      .bind("this_key", null, ScalarLens("this_key", false, None))
    base


object DynaLens:

  inline def into[T]: DynaLens[T] = ${ buildForImpl[T] }

  // ------------------------------- Macro Impl -------------------------------

  private def buildForImpl[T: Type](using q: Quotes): Expr[DynaLens[T]] =
    given Quotes = q
    import q.reflect.*

    val rootRef = ReflectOnType[T](q)(TypeRepr.of[T])(using scala.collection.mutable.Map.empty[TypedName, Boolean])

    rootRef match
      case cls: ScalaClassRef[?] if cls.isCaseClass =>
        buildFromClassRef[T](cls)
      case other =>
        report.errorAndAbort(s"DynaLens.buildForImpl only supports case classes. Got: ${other.name}")

  // Build from a case class
  private def buildFromClassRef[T: Type](cls: ScalaClassRef[?])(using q: Quotes): Expr[DynaLens[T]] =
    given Quotes = q

    // schema (runtime via class name)
    val schemaExpr: Expr[ClassType] = Expr(Schema.build(cls))

    // Build the full ClassLens tree and collect registry entries
    val (topLensExpr, regPairsExpr) = buildClassLensAndRegistry(cls, parent = None)

    '{ DynaLens[T](topLens = $topLensExpr, schema = $schemaExpr, registry = Map.from($regPairsExpr)) }


  // ----------------------- Lens + Registry Builders -------------------------

  private def fieldPairsWithParent(cls: ScalaClassRef[?], parentE: Expr[Lens])(using Quotes): Expr[List[(String, Lens)]] =
    val pairs: List[Expr[(String, Lens)]] =
      cls.fields.map { f =>
        val nameE = Expr(f.name)
        val lensE = lensForField(f.fieldRef, f.name, parent = Some(parentE))
        '{ ($nameE -> $lensE) }
      }
    Expr.ofList(pairs)

  /** Returns (classLens, registryPairs) where registryPairs is a List[(String, Lens)] keyed by class type name */
  private def buildClassLensAndRegistry(cls: ScalaClassRef[?], parent: Option[Expr[Lens]])(using Quotes): (Expr[ClassLens], Expr[List[(String, Lens)]]) =

    // Macro-generated _get / _update
    val getFn = generateGetLambdaForRef(cls)
    val updFn = generateUpdateLambdaForRef(cls)

    val nameExpr = Expr(cls.typedName.toString)
    val parentOptExpr = parent.map(p => '{ Some($p) }).getOrElse('{ None })

    // Build ClassLens with a lazy self, so children can reference `self` as their parent
    val classSchemaExpr: Expr[ClassType] = Expr(Schema.build(cls))
    val classLensExpr: Expr[ClassLens] = '{
      var self: ClassLens = null.asInstanceOf[ClassLens]
      val fieldPairs: List[(String, Lens)] = ${ fieldPairsWithParent(cls, '{ self }) }
      val fields: Map[String, Lens] = Map.from(fieldPairs)
      self = ClassLens(name = $nameExpr, isOptional = false, fields = fields, parent = $parentOptExpr, _get = $getFn, _update = $updFn, schema = $classSchemaExpr)
      self
    }

    val nestedRegs: Expr[List[(String, Lens)]] = collectNestedClassEntries(cls)
    val regPairsExpr: Expr[List[(String, Lens)]] = '{ ($nameExpr, $classLensExpr) :: $nestedRegs }

    (classLensExpr, regPairsExpr)


  // Build a Lens for a single field (Option-aware, List/Map-aware, Class vs Scalar)
  private def lensForField(ref: RTypeRef[?], fieldName: String, parent: Option[Expr[Lens]])(using Quotes): Expr[Lens] =
    import quotes.reflect.*

    def scalar(name: String, isOpt: Boolean): Expr[ScalarLens] =
      '{ ScalarLens(name = ${ Expr(name) }, isOptional = ${ Expr(isOpt) }, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None }) }) }

    ref match
      // ----- Option[...] -----
      case o: OptionRef[?] =>
        o.optionParamType match
          case s: SeqRef[?] =>
            val elLens = lensForElement(s.elementRef, parent = parent)
            '{ ListLens(name = ${ Expr(fieldName) }, isOptional = true, elementLens = $elLens, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None }) }) }

          case m: MapRef[?] =>
            val keyKindOpt = determineMapKey(m.elementRef)
            keyKindOpt match
              case Some(keyKind) =>
                val vLens = lensForElement(m.elementRef2, parent = parent)
                '{ MapLens(name = ${ Expr(fieldName) }, isOptional = true, keyKind = ${ Expr(keyKind) }, valueLens = $vLens, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None }) }) }
              case None =>
                report.errorAndAbort(s"Unsupported Map key type for field '$fieldName'")

          case e: ScalaEnumRef[?] =>
            val enumNameExpr = Expr(e.typedName.toString)
            '{ EnumLens(name = ${Expr(fieldName)}, isOptional = true, enumClassName = $enumNameExpr, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None })}) }
          case scr: ScalaClassRef[?] if scr.isCaseClass =>
            val (child, _) = buildClassLensAndRegistry(scr, parent = parent)
            // Optional class field is represented as a class lens with isOptional=true
            '{ $child.copy(isOptional = true) }

          case _ =>
            // Optional scalar
            scalar(fieldName, isOpt = true)

      // ----- Seq[...] -----
      case s: SeqRef[?] =>
        val elLens = lensForElement(s.elementRef, parent = parent)
        '{ ListLens(name = ${ Expr(fieldName) }, isOptional = false, elementLens = $elLens, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None }) }) }

      // ----- Map[K,V] -----
      case m: MapRef[?] =>
        val keyKindOpt = determineMapKey(m.elementRef)
        keyKindOpt match
          case Some(keyKind) =>
            val vLens = m.elementRef2 match
              case e: ScalaEnumRef[?] =>
                val enumNameExpr = Expr(e.typedName.toString)
                '{ EnumLens(name = ${Expr(fieldName)}, isOptional = false, enumClassName = $enumNameExpr, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None })}) }
              case _ =>
                lensForField(m.elementRef2, fieldName, parent)
            '{ MapLens(name = ${ Expr(fieldName) }, isOptional = false, keyKind = ${ Expr(keyKind) }, valueLens = $vLens, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None }) }) }
          case None =>
            report.errorAndAbort(s"Unsupported Map key type for field '$fieldName'")

      // ----- Direct case class -----
      case scr: ScalaClassRef[?] if scr.isCaseClass =>
        val (child, _) = buildClassLensAndRegistry(scr, parent = parent)
        child

      // ----- Enum -----
      case e: ScalaEnumRef[?] =>
        val enumNameExpr = Expr(e.typedName.toString)
        '{ EnumLens(name = ${Expr(fieldName)}, isOptional = ${Expr(false)}, enumClassName = $enumNameExpr, parent = ${parent.map(p => '{ Some($p) }).getOrElse('{ None })}) }

      // ----- Scalar -----
      case _ =>
        scalar(fieldName, isOpt = false)

  private def lensForElement(ref: RTypeRef[?], parent: Option[Expr[Lens]])(using Quotes): Expr[Lens] =
    import quotes.reflect.*

    ref match
      case scr: ScalaClassRef[?] if scr.isCaseClass =>
        val (child, _) = buildClassLensAndRegistry(scr, parent = parent)
        child
      case s: SeqRef[?] =>
        val el = lensForElement(s.elementRef, parent = parent)
        '{ ListLens(name = "element", isOptional = false, elementLens = $el, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None }) }) }
      case m: MapRef[?] =>
        val keyKindOpt = determineMapKey(m.elementRef)
        keyKindOpt match
          case Some(keyKind) =>
            val v = lensForElement(m.elementRef2, parent = parent)
            '{ MapLens(name = "element", isOptional = false, keyKind = ${ Expr(keyKind) }, valueLens = $v, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None }) }) }
          case None =>
            report.errorAndAbort("Unsupported Map key type for element")
      case e: ScalaEnumRef[?] =>
        val enumNameExpr = Expr(e.typedName.toString)
        '{ EnumLens(name = "element", isOptional = false, enumClassName = $enumNameExpr, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None }) }) }
      case o: OptionRef[?] =>
        o.optionParamType match
          case scr: ScalaClassRef[?] if scr.isCaseClass =>
            val (child, _) = buildClassLensAndRegistry(scr, parent = parent)
            '{ $child.copy(isOptional = true) }
          case s: SeqRef[?] =>
            val el = lensForElement(s.elementRef, parent = parent)
            '{ ListLens(name = "element", isOptional = true, elementLens = $el, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None }) }) }
          case m: MapRef[?] =>
            val keyKindOpt = determineMapKey(m.elementRef)
            keyKindOpt match
              case Some(keyKind) =>
                val v = lensForElement(m.elementRef2, parent = parent)
                '{ MapLens(name = "element", isOptional = true, keyKind = ${ Expr(keyKind) }, valueLens = $v, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None }) }) }
              case None =>
                report.errorAndAbort("Unsupported Map key type for element")
          case e: ScalaEnumRef[?] =>
            val enumNameExpr = Expr(e.typedName.toString)
            '{ EnumLens(name = "element", isOptional = true, enumClassName = $enumNameExpr, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None }) }) }
          case _ =>
            '{ ScalarLens(name = "element", isOptional = true, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None }) }) }
      case _ =>
        '{ ScalarLens(name = "element", isOptional = false, parent = ${ parent.map(p => '{ Some($p) }).getOrElse('{ None }) }) }

  // ------------------------- Registry collection ----------------------------

  private def determineMapKey(testRef: RTypeRef[?]): Option[MapKeyKind] =
    testRef match
      case p: PrimitiveRef if Set("java.lang.String", "scala.Predef.String").contains(p.name) =>
        Some(MapKeyKind.StringKey)
      case p: PrimitiveRef if Set("int", "scala.Int").contains(p.name) =>
        Some(MapKeyKind.IntKey)
      case p: PrimitiveRef if Set("long", "scala.Long").contains(p.name) =>
        Some(MapKeyKind.LongKey)
      case e: EnumRef[?] =>
        Some(MapKeyKind.EnumKey(e.typedName.toString))
      case _ =>
        None

  /** Collect `(typeName -> ClassLens)` entries for all nested case classes (one level and deeper). */
  private def collectNestedClassEntries(ref: RTypeRef[?])(using Quotes): Expr[List[(String, Lens)]] =
    ref match
      case c: ScalaClassRef[?] if c.isCaseClass =>
        val nestedLists: List[Expr[List[(String, Lens)]]] = c.fields.map(f => collectFromField(f.fieldRef))
        nestedLists.reduceOption((a, b) => '{ $a ++ $b }).getOrElse('{ Nil })
      case _ => '{ Nil }

  private def collectFromField(ref: RTypeRef[?])(using Quotes): Expr[List[(String, Lens)]] =
    ref match
      case scr: ScalaClassRef[?] if scr.isCaseClass =>
        val (child, nested) = buildClassLensAndRegistry(scr, parent = None)
        val key = Expr(scr.typedName.toString)
        val current: Expr[(String, Lens)] = '{ ($key, $child: Lens) }
        '{ $current :: $nested }
      case _ =>
        '{ Nil }

  // ----------------------------- Lambdas ------------------------------------

  /** _get for a class-like ref */
  private def generateGetLambdaForRef(ref: ScalaClassRef[?])(using Quotes): Expr[(String, Any) => ZIO[Any, DynaLensError, Any]] =
    import quotes.reflect.*

    val owner = Symbol.spliceOwner
    val mt = MethodType(List("field", "obj"))(
      _ => List(TypeRepr.of[String], TypeRepr.of[Any]),
      _ => TypeRepr.of[ZIO[Any, DynaLensError, Any]]
    )

    Lambda(owner, mt, { (_, args) =>
      val fieldParam = args(0).asInstanceOf[Term]
      val objParam   = args(1).asInstanceOf[Term]

      val tpe = ref.refType.asInstanceOf[quoted.Type[?]] match
        case '[t] => TypeRepr.of[t]
        case _    => report.errorAndAbort("Unexpected type param in _get")

      val typedObj = TypeApply(Select.unique(objParam, "asInstanceOf"), List(TypeTree.of(using tpe.asType))).asExpr.asTerm

      val cases: List[CaseDef] =
        ref.fields.map { f =>
          val nm = f.name
          val access = Select.unique(typedObj, nm)
          CaseDef(Literal(StringConstant(nm)), None, '{ ZIO.succeed(${ access.asExpr }) }.asTerm)
        }

      val fallback =
        CaseDef(Wildcard(), None,
          '{ ZIO.fail(DynaLensError("", "No such field: " + ${ fieldParam.asExprOf[String] })) }.asTerm
        )

      Match(fieldParam, cases :+ fallback)
    }).asExprOf[(String, Any) => ZIO[Any, DynaLensError, Any]]

  /** _update for a class-like ref (copy with one field changed) */
  private def generateUpdateLambdaForRef(ref: ScalaClassRef[?])(using Quotes): Expr[(String, Any, Any) => ZIO[Any, DynaLensError, Any]] =
    import quotes.reflect.*

    val owner = Symbol.spliceOwner
    val mt = MethodType(List("field", "value", "obj"))(
      _ => List(TypeRepr.of[String], TypeRepr.of[Any], TypeRepr.of[Any]),
      _ => TypeRepr.of[ZIO[Any, DynaLensError, Any]]
    )

    Lambda(owner, mt, { (_, args) =>
      val fieldParam = args(0).asInstanceOf[Term]
      val valueParam = args(1).asInstanceOf[Term]
      val objParam   = args(2).asInstanceOf[Term]

      val tpe = ref.refType.asInstanceOf[quoted.Type[?]] match
        case '[t] => TypeRepr.of[t]
        case _    => report.errorAndAbort("Unexpected type param in _update")

      val typedObj = TypeApply(Select.unique(objParam, "asInstanceOf"), List(TypeTree.of(using tpe.asType))).asExpr.asTerm

      val cases: List[CaseDef] =
        ref.fields.map { f =>
          val nm = f.name
          val ft = f.fieldRef.refType
          val castedValue = TypeApply(Select.unique(valueParam, "asInstanceOf"), List(TypeTree.of(using ft))).asExpr.asTerm

          val copyArgs = ref.fields.map { cf =>
            if cf.name == nm then NamedArg(cf.name, castedValue) else NamedArg(cf.name, Select.unique(typedObj, cf.name))
          }

          val newObj = Apply(Select.unique(typedObj, "copy"), copyArgs)

          CaseDef(Literal(StringConstant(nm)), None, '{ ZIO.succeed(${ newObj.asExpr }) }.asTerm)
        }

      val fallback =
        CaseDef(Wildcard(), None,
          '{ ZIO.fail(DynaLensError("", "No such field: " + ${ fieldParam.asExprOf[String] })) }.asTerm
        )

      Match(fieldParam, cases :+ fallback)
    }).asExprOf[(String, Any, Any) => ZIO[Any, DynaLensError, Any]]
