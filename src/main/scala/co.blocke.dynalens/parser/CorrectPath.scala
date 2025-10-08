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
package parser


object CorrectPath:

  private val segRx = "^([A-Za-z0-9_]+)(?:\\[(\\d*)\\])?(\\?)?$".r

  private def parseSeg(segStr: String): Either[String, Seg] =
    segRx.findFirstMatchIn(segStr) match
      case Some(m) =>
        val base = m.group(1)
        val idxOpt =
          Option(m.group(2)).filter(_.nonEmpty).map { raw =>
            if (raw == "") Wildcard else Fixed(raw.toInt)
          }
        val hasQ = Option(m.group(3)).isDefined
        if hasQ then
          Left(s"Optional path syntax (?) is not supported: '$segStr'")
        else
          Right(Seg(base, idxOpt, opt = false))
      case None =>
        Left(s"Invalid path segment '$segStr'")

  /** Rewrite/validate a path under current ctx (Schema/Receiver/symbols). */
  def rewritePath(rawPath: String, offset: Int)(using ctx: ExprContext): Either[DLCompileError, String] =
    inline def scopeLookup(name: String): Option[FieldType] =
      ctx.resolveSymbol(name)

    val segs = rawPath.split("\\.").toList

    @annotation.tailrec
    def loop(
              current: ClassType,
              rest: List[String],
              acc: List[String],
              isFirst: Boolean,
              inReceiver: Boolean,
              offset: Int
            )(using ExprContext): Either[DLCompileError, List[String]] =
      rest match
        case Nil => Right(acc)

        case segStr :: tail =>
          parseSeg(segStr) match
            case Left(msg) => Left(DLCompileError(offset, msg))
            case Right(seg) =>
              val segToken = seg.idx match
                case Some(Fixed(i)) => s"${seg.base}[$i]"
                case _ => seg.base

              // --- 1) Leading `this` ---
              if isFirst && seg.base == "this" then
                ctx.receiver match
                  case Some(recv) =>
                    val recvClass = ClassType("this", "this", recv.fields.values.toList)
                    return loop(
                      recvClass,
                      tail,
                      acc :+ "this",
                      isFirst = false,
                      inReceiver = true,
                      offset
                    )
                  case None =>
                    return Left(DLCompileError(offset, "Use of 'this' with no receiver in scope"))

              // --- 2) First segment: receiver field or symbol ---
              if isFirst then
                ctx.receiver.flatMap(_.fields.get(seg.base).collect { case c: ClassType => c }) match
                  case Some(childCls) =>
                    return loop(
                      childCls,
                      tail,
                      acc :+ "this" :+ seg.base,
                      isFirst = false,
                      inReceiver = true,
                      offset
                    )
                  case None => ()

                scopeLookup(seg.base) match
                  case Some(ListType(_, elem: ClassType, _)) =>
                    val collToken = s"${seg.base}"
                    return loop(
                      elem,
                      tail,
                      acc :+ collToken,
                      isFirst = false,
                      inReceiver = inReceiver,
                      offset
                    )
                  case Some(OptionType(_, inner: ListType, _)) if inner.elementType.isInstanceOf[ClassType] =>
                    val elem = inner.elementType.asInstanceOf[ClassType]
                    val collToken = s"${seg.base}"
                    return loop(
                      elem,
                      tail,
                      acc :+ collToken,
                      isFirst = false,
                      inReceiver = inReceiver,
                      offset
                    )
                  case Some(_) =>
                    return
                      if tail.nonEmpty then Left(DLCompileError(offset, s"Symbol '${seg.base}' is not a path; cannot access '${tail.head}'"))
                      else Right(acc :+ segToken)
                  case None => ()

              // --- 3) Walk schema fields ---
              current.fields.find(_.name == seg.base) match
                case Some(field) =>
                  field match
                    case c: ClassType =>
                      loop(c, tail, acc :+ seg.base, isFirst = false, inReceiver = inReceiver, offset)
                    case o: OptionType if o.valueType.isInstanceOf[ClassType] =>
                      println(s"[CorrectPath] Matched OptionType->ClassType: ${o.valueType} tail=$tail acc=$acc")
                      loop(o.valueType.asInstanceOf[ClassType], tail, acc :+ seg.base, isFirst = false, inReceiver = inReceiver, offset)
                    case l: ListType if l.elementType.isInstanceOf[ClassType] =>
                      loop(l.elementType.asInstanceOf[ClassType], tail, acc :+ seg.base, isFirst = false, inReceiver = inReceiver, offset)
                    case o: OptionType if o.valueType.isInstanceOf[ListType] =>
                      val innerList = o.valueType.asInstanceOf[ListType]
                      println("[Here in OptionType of CorrectPath] " + o + " inner: " + innerList)

                      innerList.elementType match
                        case c: ClassType =>
                          // Option[List[ClassType]] – continue descending
                          loop(c, tail, acc :+ segToken, isFirst = false, inReceiver = inReceiver, offset)

                        case _: ScalarType =>
                          // Option[List[Scalar]] – allow indexing, but nothing beyond
                          if tail.isEmpty then Right(acc :+ segToken)
                          else Left(DLCompileError(offset, s"Cannot navigate inside list of scalar field '${seg.base}'"))

                        case _ =>
                          Left(DLCompileError(offset, s"Unsupported Option[List] element type for field '${seg.base}'"))
                    case _ =>
                      if tail.isEmpty then Right(acc :+ segToken)
                      else Left(DLCompileError(offset, s"Field '${seg.base}' is not a nested object"))
                case None =>
                  if isFirst then
                    scopeLookup(seg.base) match
                      case Some(_) =>
                        if tail.nonEmpty then
                          Left(DLCompileError(offset, s"Symbol '${seg.base}' is not a path; cannot access '${tail.head}'"))
                        else Right(acc :+ segToken)
                      case None =>
                        Left(DLCompileError(offset,
                          s"Field '${seg.base}' does not exist in schema, receiver, or symbol scope"))
                  else
                    Left(DLCompileError(offset, s"Field '${seg.base}' does not exist here"))

    loop(ctx.schema, segs, Nil, isFirst = true, inReceiver = false, offset).map(_.mkString("."))