/*
 * Copyright (c) 2025 Greg Zoller
 * This software is licensed under the MIT License (see LICENSE file).
 */

package co.blocke.dynalens
package parser

import fastparse.*
import zio.*

object Parser:

  def parseScript(script: String): ZIO[Any, DynaLensError, BlockStmt] =
    ZIO.attempt {
      val result = parse(script, script => Grammar.topLevelBlock(using script))
      result match {
        case Parsed.Success(ast, _) => ast
        case f: Parsed.Failure      => throw new RuntimeException(f.trace().longMsg)
      }
    }.mapError(ex => DynaLensError(s"Parse error: ${ex.getMessage}"))

  def parseScriptNoZIO(script: String): Either[DynaLensError, BlockStmt] =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(
        parseScript(script).either
      ).getOrThrow()
    }