/*
 * Copyright (c) 2025 Greg Zoller
 * This software is licensed under the MIT License (see LICENSE file).
 */

package co.blocke.dynalens

import zio._
import zio.test._

import DynaLens.*

object SimpleStatementsSpec extends ZIOSpecDefault:

  val rootAssignr = dynalens[Item]

  val sampleItem = Item("ABC", 10)

  def spec = suite("SimpleStatementsSpec")(
    test("ValStmt should add a symbol to context") {
      val stmt = ValStmt("v1", ConstantFn(123))
      for {
        newCtx <- stmt.resolve(Map.empty)
      } yield assertTrue(newCtx("v1")._1 == 123)
    },
    test("MapStmt should update object field") {
      val stmt = MapStmt("qty", ConstantFn(5))
      for {
        ctx <- ZIO.succeed(Map("top" -> (sampleItem, rootAssignr)))
        newCtx <- stmt.resolve(ctx)
      } yield assertTrue(newCtx("top")._1.asInstanceOf[Item].qty == 5)
    },
    test("IfStmt true branch executes") {
      val stmt = IfStmt(
        GreaterThanFn(ConstantFn(10), ConstantFn(5)),
        BlockStmt(List(UpdateStmt("qty", ConstantFn(7)))),
        Some(BlockStmt(List(UpdateStmt("qty", ConstantFn(3)))))
      )
      for {
        ctx <- ZIO.succeed(Map("top" -> (sampleItem, rootAssignr)))
        newCtx <- stmt.resolve(ctx)
      } yield assertTrue(newCtx("top")._1.asInstanceOf[Item].qty == 7)
    },
    test("IfStmt false branch executes") {
      val stmt = IfStmt(
        GreaterThanFn(ConstantFn(1), ConstantFn(5)),
        BlockStmt(List(UpdateStmt("qty", ConstantFn(7)))),
        Some(BlockStmt(List(UpdateStmt("qty", ConstantFn(3)))))
      )
      for {
        ctx <- ZIO.succeed(Map("top" -> (sampleItem, rootAssignr)))
        newCtx <- stmt.resolve(ctx)
      } yield assertTrue(newCtx("top")._1.asInstanceOf[Item].qty == 3)
    }
  ).provide(BiMapRegistry.layer(EmptyBiMapRegistry))
