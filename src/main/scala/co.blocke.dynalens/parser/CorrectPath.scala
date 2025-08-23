package co.blocke.dynalens
package parser

object CorrectPath:

  // segRx/Seg/Idx/Wildcard/Fixed are assumed defined elsewhere in your codebase

  private def parseSeg(name: String): Either[String, Seg] = name match
    case segRx(base, idxStr, optStr) =>
      val idx =
        if (idxStr eq null) None
        else if (idxStr.isEmpty) Some(Wildcard)
        else Some(Fixed(idxStr.toInt))
      Right(Seg(base, idx, opt = optStr != null))
    case _ =>
      Left(s"Invalid path segment '$name'")

  private def render(base: String, idx: Option[Idx], opt: Boolean): String =
    val idxTxt = idx match
      case Some(Wildcard) => "[]"
      case Some(Fixed(i)) => s"[$i]"
      case None => ""
    val optTxt = if opt then "?" else ""
    s"$base$idxTxt$optTxt"

  // Force-correct suffix based on expected type, but preserve a fixed index when list-typed
  private def correct(seg: Seg, expectedType: String): Either[String, String] =
    expectedType match
      case "[]" =>
        seg.idx match
          case Some(Fixed(i)) => Right(render(seg.base, Some(Fixed(i)), opt = false))
          case _              => Right(render(seg.base, Some(Wildcard), opt = false))
      case "[]?" =>
        seg.idx match
          case Some(Fixed(i)) => Right(render(seg.base, Some(Fixed(i)), opt = true))
          case _              => Right(render(seg.base, Some(Wildcard), opt = true))
      case "{}" | "{}?" | "?" | "" =>
        // Non-list types may not be indexed
        seg.idx match
          case Some(_) => Left(s"Cannot index into non-list field '${seg.base}'")
          case None =>
            val opt = expectedType.endsWith("?") || expectedType == "?"
            Right(render(seg.base, None, opt))
      case _ =>
        Right(render(seg.base, seg.idx, seg.opt)) // fallback

  private inline def expectedOf(node: Any): String =
    node match
      case m: Map[?, ?] @unchecked =>
        m.asInstanceOf[Map[String, Any]].get("__type").collect { case s: String => s }.getOrElse("{}")
      case s: String => s
      case _         => ""

  private inline def nextNode(node: Any): Map[String, Any] =
    node match
      case m: Map[?, ?] @unchecked => m.asInstanceOf[Map[String, Any]]
      case _                       => Map.empty[String, Any]

  /** Rewrite/validate a path under current ctx.
   * - First tries absolute lookup in ctx.typeInfo.
   * - If first segment not found and a receiver exists, treat bare name as relative to receiver and prefix `this.`.
   * - Allows top-level `val` symbols recorded in ctx.sym to pass through unchanged (and they are terminal).
   */
  def rewritePath(rawPath: String, offset: Int)(using ctx: ExprContext): Either[DLCompileError, String] =
    val segs = rawPath.split("\\.").toList

    @annotation.tailrec
    def loop(current: Map[String, Any],
             rest: List[String],
             acc: List[String],
             isFirst: Boolean,
             inReceiver: Boolean): Either[DLCompileError, List[String]] =
      rest match
        case Nil => Right(acc)

        case segStr :: tail =>
          parseSeg(segStr) match
            case Left(msg) => Left(DLCompileError(offset, msg))

            case Right(seg) =>
              // explicit `this` still handled first
              if isFirst && seg.base == "this" then
                ctx.receiver match
                  case None =>
                    Left(DLCompileError(offset, "Use of 'this' with no receiver in scope"))
                  case Some(recv) =>
                    // Start inside receiver fields; `this` itself has no suffix
                    loop(nextNode(recv.fields), tail, acc :+ "this", isFirst = false, inReceiver = true)
              else {
                // ---- NEW: prefer receiver/scopes on the *first* segment ----
                if isFirst then
                  val scopeTop: Map[String, Any] =
                    ctx.scopes.headOption.getOrElse(Map.empty)
          
                  val recvMap: Map[String, Any] =
                    ctx.receiver.map(r => nextNode(r.fields)).getOrElse(Map.empty)
          
                  // If the name exists in scope or receiver, treat it as relative: `name` -> `this.name`
                  scopeTop.get(seg.base).orElse(recvMap.get(seg.base)) match
                    case Some(node) =>
                      correct(seg, expectedOf(node)) match
                        case Left(msg) => Left(DLCompileError(offset, msg))
                        case Right(spelled) =>
                          // prefix `this` so downstream sees explicit receiver
                          return loop(
                            nextNode(node),
                            tail,
                            acc :+ "this" :+ spelled,
                            isFirst = false,
                            inReceiver = true
                          )
                    case None => () // fall through
          
                // ---- ORIGINAL flow continues: absolute lookup, then symbols, then errors ----
                current.get(seg.base) match
                  // Absolute hit in current schema (typeInfo or subtree)
                  case Some(node) =>
                    correct(seg, expectedOf(node)) match
                      case Left(msg) => Left(DLCompileError(offset, msg))
                      case Right(spelled) =>
                        loop(nextNode(node), tail, acc :+ spelled, isFirst = false, inReceiver = inReceiver)
          
                  case None =>
                    if isFirst then
                      // top-level vals/symbols?
                      ctx.resolveSymbol(seg.base) match
                        case Some(_) =>
                          // a val is terminal; forbid chaining further segments
                          if tail.nonEmpty then
                            Left(DLCompileError(offset, s"Symbol '${seg.base}' is not a path; cannot access '${tail.head}'"))
                          else
                            Right(acc :+ segStr)
                        case None =>
                          Left(DLCompileError(offset, s"Field '${seg.base}' does not exist in typeInfo or receiver"))
                    else
                      Left(DLCompileError(offset, s"Field '${seg.base}' does not exist here"))
              }            

    loop(ctx.typeInfo, segs, Nil, isFirst = true, inReceiver = false).map(_.mkString("."))