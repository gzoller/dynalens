package co.blocke.dynalens
package parser

import fastparse.*
import NoWhitespace.*
import Grammar.*
import DynaLens.*
import zio.*

object Parser:

  def parseScript(script: String): BlockStmt = {
    val result = parse(script, script => Grammar.topLevelBlock(using script))
    result match {
      case Parsed.Success(ast, _) => ast
      case f: Parsed.Failure      => throw new RuntimeException(f.trace().longMsg)
    }
  }

  inline def run[T]( script: BlockStmt, target: T ): ZIO[Any,DynaLensError,(T,Map[String,(Any,DynaLens[?])])] =
    val ar = dynalens[T]
    val ctx = Map("this" -> (target,ar))
    for {
      resultCtx <- script.resolve(ctx)  // Map[String, (Any, Assignr[?])]
      (resultObj, _) = resultCtx("this")
    } yield (resultObj.asInstanceOf[T], resultCtx)
