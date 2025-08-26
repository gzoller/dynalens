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
        m.asInstanceOf[Map[String, Any]].get("__type") match
          case Some(s: String) => s
          case _               => "{}"
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
    def scopeLookup(name: String): Option[Any] =
      ctx.scopes.collectFirst { case m if m.contains(name) => m(name) }

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
              if isFirst && seg.base == "this" then
                ctx.receiver match
                  case None => Left(DLCompileError(offset, "Use of 'this' with no receiver in scope"))
                  case Some(recv) =>
                    loop(nextNode(recv.fields), tail, acc :+ "this", isFirst=false, inReceiver=true)
              else {
                if isFirst then
                  // 1) receiver-relative bare field?  (prefer this FIRST)
                  val recvMap = ctx.receiver.map(r => nextNode(r.fields)).getOrElse(Map.empty)
                  if recvMap.contains(seg.base) then
                    val node = recvMap(seg.base)
                    correct(seg, expectedOf(node)) match
                      case Left(msg) => return Left(DLCompileError(offset, msg))
                      case Right(spelled) =>
                        // prefix explicit `this` and enter receiver subtree
                        return loop(
                          nextNode(node),
                          tail,
                          acc :+ "this" :+ spelled,
                          isFirst = false,
                          inReceiver = true
                        )

                  // 2) loop-scope symbol? (only if not a receiver field)
                  scopeLookup(seg.base) match
                    case Some(node) =>
                      // Bare loop name (e.g., "items") should mean the whole collection: rewrite to "items[]"
                      val collToken = s"${seg.base}[]"
                      // We don't introduce `this` here; this is a collection reference.
                      // Keep `nextNode(node)` so any following segments still see the element schema.
                      return loop(
                        nextNode(node),
                        tail,
                        acc :+ collToken,
                        isFirst = false,
                        inReceiver = inReceiver
                      )

                    case None => ()

                current.get(seg.base) match
                  case Some(node) =>
                    correct(seg, expectedOf(node)) match
                      case Left(msg) => Left(DLCompileError(offset, msg))
                      case Right(spelled) =>
                        loop(nextNode(node), tail, acc :+ spelled, isFirst=false, inReceiver=inReceiver)
                  case None =>
                    if isFirst then
                      ctx.resolveSymbol(seg.base) match
                        case Some(_) =>
                          if tail.nonEmpty then
                            Left(DLCompileError(offset, s"Symbol '${seg.base}' is not a path; cannot access '${tail.head}'"))
                          else Right(acc :+ segStr)
                        case None =>
                          Left(DLCompileError(offset, s"Field '${seg.base}' does not exist in typeInfo, receiver, or loop scope"))
                    else
                      Left(DLCompileError(offset, s"Field '${seg.base}' does not exist here"))
              }

    val res = loop(ctx.typeInfo, segs, Nil, isFirst=true, inReceiver=false).map(_.mkString("."))
    res