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
