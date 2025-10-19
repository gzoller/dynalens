package co.blocke.dynalens
package fn

import zio.*
import scala.annotation.tailrec


case class UUIDFn(posStr: String) extends Fn[Any]:
  override val methodName = "uuid"
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ZIO.succeed(java.util.UUID.randomUUID().toString)


case class ElseFn(recv: Fn[Any], default: Fn[Any], posStr: String) extends MethodFn[Any]:
  override val methodName = "else"
  override def args: List[Fn[Any]] = List(default)

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    copy(recv = kids(0).asInstanceOf[Fn[Any]], default = kids(1).asInstanceOf[Fn[Any]])

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for
      optVal <- recv.resolve(ctx)
      res <- optVal match
        case o: Option[?] =>
          o match
            case Some(v) => ZIO.succeed(v)
            case None    => default.resolve(ctx)
        case null =>
          default.resolve(ctx)
        case other =>
          ZIO.fail(DynaLensError(posStr, s"else() requires Option receiver, got ${other.getClass.getSimpleName}"))
    yield res


case class BlockFn[R](statements: Seq[Statement], finalFn: Fn[R], posStr: String) extends Fn[R]:
  override val methodName: String = "{block}"

  // A block doesn't have a meaningful runtime receiver; it's a sequence of statements
  // culminating in a final expression. Treat the root as the receiver.
  override def recv: Fn[Any] = RootFn

  // Only the final function is a child argument in the expression tree.
  override def args: List[Fn[Any]] = List(finalFn.asInstanceOf[Fn[Any]])

  override def children: List[Fn[?]] = List(finalFn)

  // Rebuild with a (single) final function child.
  override def rebuild(kids: List[Fn[?]]): Fn[R] =
    kids match
      case f :: Nil => copy(finalFn = f.asInstanceOf[Fn[R]])
      case _        => this

  // Block optionality mirrors the final expression's optionality.
  override val isOptional: Boolean = finalFn.isOptional

  // Execute statements in order, threading the DynaContext; return the value of the final Fn.
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, R] =
    val staged: ZIO[_BiMapRegistry, DynaLensError, DynaContext] =
      statements.foldLeft(ZIO.succeed(ctx): ZIO[_BiMapRegistry, DynaLensError, DynaContext]) {
        (acc, stmt) => acc.flatMap(ctx2 => stmt.resolve(ctx2))
      }
    staged.flatMap(finalFn.resolve)


case class CaseWhenFn(
                       receiver: Fn[Any],
                       cases: Vector[(Any, Fn[Any])],
                       default: Option[Fn[Any]],
                       permissive: Boolean = false,
                       posStr: String
                     ) extends Fn[Any]:

  override val recv: Fn[Any] = receiver
  override def args: List[Fn[Any]] =
    receiver :: (cases.map(_._2).toList ++ default.toList)

  override def children: List[Fn[?]] = args

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    val rcv = kids.head.asInstanceOf[Fn[Any]]
    val rhsCount = cases.length
    val newCases =
      cases.indices.map(i => (cases(i)._1, kids(i + 1).asInstanceOf[Fn[Any]])).toVector
    val newDefault =
      if default.isDefined then Some(kids(rhsCount + 1).asInstanceOf[Fn[Any]]) else None
    copy(receiver = rcv, cases = newCases, default = newDefault)

  override val isOptional: Boolean =
    receiver.isOptional || cases.exists(_._2.isOptional) || default.exists(_.isOptional)

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for
      v <- receiver.resolve(ctx)
      result <- cases.collectFirst { case (p, rhs) if v == p => rhs.resolve(ctx) } match
        case Some(matched) => matched
        case None =>
          default match
            case Some(defFn) => defFn.resolve(ctx)
            case None if permissive => ZIO.succeed(v)
            case None =>
              ZIO.fail(DynaLensError(posStr, s"No case matched for value: $v"))
    yield result


case class IndexFn(recv: Fn[Any], index: Fn[Any], posStr: String)
  extends Fn[Any]
    with MethodFn[Any] {

  override val methodName: String = "[index]"

  // `MethodFn` default isOptional uses recv/args optionality; that’s OK.
  // (Map indexing returns Option[...] at value level — type system communicates that.)

  override def args: List[Fn[Any]] = List(index)

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    kids match
      case r :: i :: Nil => copy(recv = r.asInstanceOf[Fn[Any]], index = i.asInstanceOf[Fn[Any]])
      case r :: Nil      => copy(recv = r.asInstanceOf[Fn[Any]])
      case _             => this

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    for {
      rv <- recv.resolve(ctx)
      ix <- index.resolve(ctx)
      out <- resolveOn(rv, ix)
    } yield out

  private def resolveOn(receiverValue: Any, indexValue: Any): ZIO[_BiMapRegistry, DynaLensError, Any] = {
    // --- Option / null unwrapping for the receiver ---
    receiverValue match {
      case null        => ZIO.fail(DynaLensError(posStr, s"indexing on null receiver"))
      case None        =>
        // Option[List] or Option[Map] empty -> result is None (fits type rules)
        ZIO.succeed(None)
      case Some(inner) =>
        // recurse on inner; be careful with Option[Map] => Option[Option[V]] semantics
        resolveOn(inner, indexValue).map {
          // If inner produced Option[V], wrapping here yields Option[Option[V]] automatically
          case v => Some(v)
        }

      // ---- Map[K,V] => Option[V] ----
      case m: scala.collection.immutable.Map[?, ?] =>
        ZIO.attempt {
          m.asInstanceOf[Map[Any, Any]].get(indexValue) // Option[V]
        }.mapError(e => DynaLensError(posStr, s"map indexing failed: ${e.getMessage}"))

      // ---- Iterable/Seq => element at Int (fail on OOB) ----
      case it: Iterable[?] =>
        // Expect an integral index
        val idxEither = indexAsInt(indexValue)
        idxEither match
          case Left(msg) => ZIO.fail(DynaLensError(posStr, msg))
          case Right(i) =>
            val vec = it.toVector.asInstanceOf[Vector[Any]]
            if (i < 0 || i >= vec.length)
              ZIO.fail(DynaLensError(posStr, s"index $i out of bounds (size=${vec.length})"))
            else
              ZIO.succeed(vec(i))

      // ---- Unsupported receiver types ----
      case other =>
        ZIO.fail(DynaLensError(posStr,
          s"indexing not supported for ${other.getClass.getSimpleName}; expected List/Seq or Map (or Option thereof)"
        ))
    }
  }

  private def indexAsInt(ix: Any): Either[String, Int] = ix match {
    case null                 => Left("index is null")
    case i: java.lang.Integer => Right(i.intValue)
    case l: java.lang.Long    =>
      if (l >= Int.MinValue && l <= Int.MaxValue) Right(l.toInt)
      else Left(s"index out of Int range: $l")
    case s: java.lang.Short   => Right(s.toInt)
    case b: java.lang.Byte    => Right(b.toInt)
    case bi: java.math.BigInteger =>
      val asLong = try bi.longValueExact() catch { case _: ArithmeticException => Long.MaxValue }
      if (asLong >= Int.MinValue && asLong <= Int.MaxValue) Right(asLong.toInt)
      else Left(s"index out of Int range: $bi")
    case d: java.lang.Double  => Right(d.toInt) // permissive cast; you may want to reject non-integral doubles
    case f: java.lang.Float   => Right(f.toInt)
    case other                => Left(s"index must be numeric, got ${other.getClass.getSimpleName}")
  }
}


/** Absolute base receiver for standalone or constant expressions. */
case object RootFn extends Fn[Any]:
  // Since RootFn is the origin of all trees, its receiver is itself.
  override val recv: Fn[Any] = this
  override val args: List[Fn[Any]] = Nil
  override val methodName: String = "<root>"
  override val posStr: String = "<root>"
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ZIO.succeed(())


case object NoneFn extends Fn[Any]:
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  override val methodName: String = "<none>"
  val posStr = ""
  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, None.type] =
    ZIO.succeed(None)

case class BooleanConstantFn(value: Boolean) extends Fn[Boolean]:
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  override val methodName: String = "<bool>"
  val posStr = ""
  override def rebuild(kids: List[Fn[?]]): Fn[Boolean] = this
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Boolean] =
    ZIO.succeed(value)

case class ConstantFn[R](out: R) extends Fn[R]:
  override val recv: Fn[Any] = RootFn
  override val args: List[Fn[Any]] = Nil
  override val methodName: String = "<const>"
  val posStr = ""
  override def rebuild(kids: List[Fn[?]]): Fn[R] = this
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, R] =
    ZIO.succeed(out)


case class Tuple2Fn(recv: Fn[Any], args: List[Fn[Any]], posStr: String)
  extends BinaryFn[Any]:

  override val methodName: String = "<tuple2>"
  override val isOptional: Boolean =
    recv.isOptional || args.exists(_.isOptional)

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    kids match
      case r :: a :: Nil =>
        copy(recv = r.asInstanceOf[Fn[Any]], args = List(a.asInstanceOf[Fn[Any]]))
      case _ => this

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, (Any, Any)] =
    for
      a <- recv.resolve(ctx)
      b <- args.head.resolve(ctx)
    yield (a, b)


case class LoopFn(inner: Fn[Any], posStr: String)
  extends Fn[List[Any]]:

  override val recv: Fn[Any] = RootFn               // implicit receiver: "this"
  override def args: List[Fn[Any]] = List(inner)    // body of the loop
  override val methodName: String = "<loop>"

  override val isOptional: Boolean = inner.isOptional

  override def children: List[Fn[?]] = List(inner)
  override def rebuild(kids: List[Fn[?]]): Fn[List[Any]] =
    kids match
      case h :: Nil => copy(inner = h.asInstanceOf[Fn[Any]])
      case _        => this

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, List[Any]] =
    ctx.get("this") match
      case Some((raw, lens)) =>
        for
          seq <- ZIO.fromEither(FnUtils.asSeq(raw, "loop", posStr))
            .mapError(_ => DynaLensError(posStr, s"LoopFn expected an Iterable for 'this', got ${raw.getClass.getSimpleName}"))
          results <- ZIO.foreach(seq) { item =>
            val localCtx = ctx.clone.addOne("this", (item, lens))
            inner.resolve(localCtx)
          }
        yield results.toList

      case None =>
        ZIO.fail(DynaLensError(posStr, "LoopFn requires 'this' bound to a collection"))


case class PolyFn(recv: Fn[Any], args: List[Fn[Any]], posStr: String) extends Fn[Any]:
  override val methodName: String = "<poly>"
  override val isOptional: Boolean =
    recv.isOptional || args.exists(_.isOptional)
  override def children: List[Fn[?]] =
    recv :: args

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    kids match
      case r :: tail =>
        copy(recv = r.asInstanceOf[Fn[Any]], args = tail.asInstanceOf[List[Fn[Any]]])
      case _ =>
        this

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ctx.get("this") match
      case Some((_, lens)) =>
        for
          _ <- ZIO.foreachDiscard(args) { fn =>
            fn.resolve(ctx).map(res => ctx.put("this", (res, lens)))
          }
          finalVal <- ZIO.attempt(ctx("this")._1)
            .mapError(e => DynaLensError(posStr, e.getMessage))
        yield finalVal

      case None =>
        ZIO.fail(DynaLensError(posStr, "'this' not found in context"))


case class GetFn(
                  path: String,
                  override val isOptional: Boolean,
                  recv: Fn[Any],     // always defined, never None
                  posStr: String
                ) extends Fn[Any]:

  import Path.* // for parsePath, PathElement, Field, IndexedField, partialPath

  override val args: List[Fn[Any]] = Nil
  override val methodName: String = "<get>"

  override def rebuild(kids: List[Fn[?]]): Fn[Any] =
    kids match
      case r :: Nil => copy(recv = r.asInstanceOf[Fn[Any]])
      case _        => this

  // ------------------------------------------------------------------
  // Helpers for soft-missing and soft-index semantics
  // ------------------------------------------------------------------
  private inline def softMissing[A](zio: ZIO[_BiMapRegistry, DynaLensError, A])
  : ZIO[_BiMapRegistry, DynaLensError, Any] =
    if isOptional then
      zio.either.flatMap {
        case Right(v) => ZIO.succeed(v)
        case Left(_)  => ZIO.succeed(None)
      }
    else zio.asInstanceOf[ZIO[_BiMapRegistry, DynaLensError, Any]]

  private inline def softIndex[A](zio: => ZIO[_BiMapRegistry, DynaLensError, A])
  : ZIO[_BiMapRegistry, DynaLensError, Any] =
    if isOptional then
      zio.either.map(_.toOption).map(_.getOrElse(None))
    else zio.asInstanceOf[ZIO[_BiMapRegistry, DynaLensError, Any]]

  // ------------------------------------------------------------------
  // Core resolve entry
  // ------------------------------------------------------------------
  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    val parts = parsePath(path)

    recv match
      // --- Treat NoOpFn as root-level / implicit context (unanchored)
      case NoOpFn =>
        resolveUnanchored(parts, ctx)

      // --- Root-level / implicit context
      case RootFn =>
        resolveUnanchored(parts, ctx)

      // --- Anchored path: explicit receiver value
      case other =>
        for
          base   <- other.resolve(ctx)
          result <- resolveFromBase(base, parts, ctx)
        yield result

  // ------------------------------------------------------------------
  // Resolve relative to known base value
  // ------------------------------------------------------------------
  private def resolveFromBase(base: Any, segs: List[PathElement], ctx: DynaContext)
  : ZIO[_BiMapRegistry, DynaLensError, Any] =
    base match
      case null | None =>
        if isOptional then ZIO.succeed(None)
        else ZIO.fail(DynaLensError(posStr, s"Cannot resolve '$path' on null/None receiver"))
      case Some(inner) =>
        resolveFromBase(inner, segs, ctx)
      case _ =>
        softMissing(ZIO.fromEither(walk(base, segs)))

  // ------------------------------------------------------------------
  // Resolve from current or top-level context
  // ------------------------------------------------------------------
  private def resolveUnanchored(parts: List[PathElement], ctx: DynaContext)
  : ZIO[_BiMapRegistry, DynaLensError, Any] = parts match

    // --- this.xxx
    case Field("this") :: rest =>
      ctx.get("this") match
        case Some((root, Some(l: DynaLens[?]))) if rest.nonEmpty =>
          val lens = l.asInstanceOf[DynaLens[Any]]
          softMissing(lens.get(partialPath(rest), root.asInstanceOf[lens.ThisT]))
        case Some((root, _)) =>
          softMissing(ZIO.fromEither(walk(root, rest)))
        case None =>
          ZIO.fail(DynaLensError(posStr, "Use of 'this' with no receiver in scope"))

    // --- bound symbol
    case first :: rest if ctx.contains(first.name) =>
      ctx.get(first.name) match
        case Some((v, Some(boundLens: DynaLens[?]))) =>
          val lens = boundLens.asInstanceOf[DynaLens[Any]]
          if rest.isEmpty then ZIO.succeed(v)
          else softMissing(lens.get(partialPath(rest), v.asInstanceOf[lens.ThisT]))

        case Some((v, None)) =>
          if rest.isEmpty then ZIO.succeed(v)
          else softMissing(ZIO.fromEither(walk(v, rest)))

        case None =>
          ZIO.fail(DynaLensError(posStr, s"Field ${first.name} not found in context"))

    // --- element-relative path
    case segs @ (first :: _) if ctx.contains("this") && !ctx.contains(first.name) =>
      ctx.get("this") match
        case Some((root, Some(l: DynaLens[?]))) =>
          val lens = l.asInstanceOf[DynaLens[Any]]
          softMissing(lens.get(Path.partialPath(segs), root.asInstanceOf[lens.ThisT]))
        case Some((root, None)) =>
          softMissing(ZIO.fromEither(walk(root, segs)))
        case None =>
          ZIO.fail(DynaLensError(posStr, "Use of element-relative path but no 'this' in context"))

    // --- default: top
    case _ =>
      ctx.get("top") match
        case Some((obj, Some(l: DynaLens[?]))) =>
          val lens = l.asInstanceOf[DynaLens[Any]]
          softMissing(lens.get(path, obj.asInstanceOf[lens.ThisT]))
        case Some((obj, None)) =>
          softMissing(ZIO.fromEither(walk(obj, parts)))
        case _ =>
          if isOptional then ZIO.succeed(None)
          else ZIO.fail(DynaLensError(posStr, s"Missing 'top' in context for path '$path'"))

  // ------------------------------------------------------------------
  // Object traversal
  // ------------------------------------------------------------------
  @tailrec
  private def walk(obj: Any, parts: List[PathElement]): Either[DynaLensError, Any] =
    def fieldOf(p: Product, name: String): Option[Any] =
      val names = p.productElementNames.iterator
      var i = 0
      while names.hasNext do
        if names.next() == name then return Some(p.productElement(i))
        i += 1
      None

    parts match
      case Nil => Right(obj)

      case Field(name) :: tail =>
        val nextOpt = obj match
          case m: Map[?, ?] => m.asInstanceOf[Map[String, Any]].get(name)
          case p: Product   => fieldOf(p, name)
          case _            => None
        nextOpt match
          case Some(next) => walk(next, tail)
          case None       => Left(DynaLensError(posStr, s"Field not found: '$name'"))

      case IndexedField(name, idxOpt) :: tail =>
        val collOpt = obj match
          case m: Map[?, ?] => m.asInstanceOf[Map[String, Any]].get(name)
          case p: Product   => fieldOf(p, name)
          case _            => None

        collOpt match
          case None => Left(DynaLensError(posStr, s"Field not found: '$name'"))
          case Some(coll) =>
            (coll, idxOpt) match
              // ---- Indexed sequence ----
              case (xs: Seq[?], Some(idxStr)) =>
                val s = xs.asInstanceOf[Seq[Any]]
                idxStr.toIntOption match
                  case Some(i) if i >= 0 && i < s.length =>
                    walk(s(i), tail)
                  case Some(i) =>
                    Left(DynaLensError(posStr, s"Index $i out of bounds for field '$name'"))
                  case None =>
                    Left(DynaLensError(posStr, s"Non-numeric index '$idxStr' used on sequence field '$name'"))

              // ---- Map key access ----
              case (m: Map[?, ?], Some(keyStr)) =>
                val mm = m.asInstanceOf[Map[Any, Any]]
                mm.get(keyStr) match
                  case Some(v) => walk(v, tail)
                  case None => Left(DynaLensError(posStr, s"Key '$keyStr' not found in Map '$name'"))

              // ---- Wildcard access (no specific index) ----
              case (_: Seq[?], None) =>
                Left(DynaLensError(posStr, s"Wildcard index not allowed for '$name'"))

              case (_: Map[?, ?], None) =>
                Left(DynaLensError(posStr, s"Wildcard key access not allowed for '$name'"))

              // ---- Fallback ----
              case _ =>
                Left(DynaLensError(posStr, s"Field '$name' is not indexable"))


object NoOpFn extends Fn[Any]:
  override val methodName: String = "<noop>"
  override val recv: Fn[Any] = this
  override val args: List[Fn[Any]] = Nil
  override val posStr: String = "<noop>"

  override def children: List[Fn[?]] = Nil

  override def rebuild(kids: List[Fn[?]]): Fn[Any] = this

  override def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, Any] =
    ZIO.fail(DynaLensError(posStr, "NoOpFn should never be resolved"))

