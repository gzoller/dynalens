package co.blocke.dynalens.parser

object CorrectPath:

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
          case _ => Right(render(seg.base, Some(Wildcard), opt = false))
      case "[]?" =>
        seg.idx match
          case Some(Fixed(i)) => Right(render(seg.base, Some(Fixed(i)), opt = true))
          case _ => Right(render(seg.base, Some(Wildcard), opt = true))
      case "{}" | "{}?" | "?" | "" =>
        // Non-list types may not be indexed
        seg.idx match
          case Some(_) => Left(s"Cannot index into non-list field '${seg.base}'")
          case None =>
            val opt = expectedType.endsWith("?") || expectedType == "?"
            Right(render(seg.base, None, opt))
      case _ =>
        Right(render(seg.base, seg.idx, seg.opt)) // fallback


  def rewritePath(rawPath: String, offset: Int)(using ctx: ExprContext): Either[DLCompileError, String] =
    val segments = rawPath.split("\\.").toList

    @annotation.tailrec
    def loop(current: Map[String, Any], segs: List[String], acc: List[String], isFirst: Boolean): Either[DLCompileError, List[String]] =
      segs match
        case Nil => Right(acc)

        case segStr :: rest =>
          parseSeg(segStr) match
            case Left(msg) =>
              Left(DLCompileError(offset, msg))

            case Right(seg) =>
              // Absolute lookup first
              current.get(seg.base) match
                case None =>
                  // If first segment and searchThis enabled, try relativeFields
                  if isFirst && (ctx.searchThis || seg.base == "this") && ctx.relativeFields.nonEmpty then
                    ctx.relativeFields.get(seg.base) match
                      case Some(subtree: Map[String @unchecked, Any @unchecked]) =>
                        val expectedType = subtree.get("__type").collect { case s: String => s }.getOrElse("{}")
                        correct(seg, expectedType) match
                          case Left(msg) => Left(DLCompileError(offset, msg))
                          case Right(spelled) => loop(subtree, rest, acc :+ spelled, isFirst = false)

                      case Some(expectedType: String) =>
                        correct(seg, expectedType) match
                          case Left(msg) => Left(DLCompileError(offset, msg))
                          case Right(spelled) => loop(Map.empty, rest, acc :+ spelled, isFirst = false)

                      case None =>
                        // allow top-level val symbols
                        val valKey = s"__val_${seg.base}"
                        if ctx.typeInfo.contains(valKey) then
                          loop(current, rest, acc :+ segStr, isFirst = false)
                        else
                          Left(DLCompileError(offset, s"Field '${seg.base}' does not exist in typeInfo or relativeFields"))

                  else
                    // No relative match — allow top-level val symbols
                    val valKey = s"__val_${seg.base}"
                    if ctx.typeInfo.contains(valKey) then
                      loop(current, rest, acc :+ segStr, isFirst = false)
                    else
                      Left(DLCompileError(offset, s"Field '${seg.base}' does not exist in typeInfo"))

                case Some(subtree: Map[String @unchecked, Any @unchecked]) =>
                  val expectedType = subtree.get("__type").collect { case s: String => s }.getOrElse("{}")
                  correct(seg, expectedType) match
                    case Left(msg) => Left(DLCompileError(offset, msg))
                    case Right(spelled) => loop(subtree, rest, acc :+ spelled, isFirst = false)

                case Some(expectedType: String) =>
                  correct(seg, expectedType) match
                    case Left(msg) => Left(DLCompileError(offset, msg))
                    case Right(spelled) => loop(Map.empty, rest, acc :+ spelled, isFirst = false)

                case _ =>
                  Left(DLCompileError(offset, s"Unexpected structure for field '${seg.base}'"))

    loop(ctx.typeInfo, segments, Nil, isFirst = true).map(_.mkString("."))
