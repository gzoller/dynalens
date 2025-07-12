package co.blocke.dynalens

import zio.*
import Path.*

trait Statement:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Map[String, (Any, DynaLens[?])]]

case class ValStmt[R](name: String, fn: Fn[R]) extends Statement:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Map[String, (Any, DynaLens[?])]] =
    for {
      value <- fn.resolve(ctx)
    } yield ctx + (name -> (value, null))

case class MapStmt(path: String, fn: Fn[?]) extends Statement:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Map[String, (Any, DynaLens[?])]] =
    ctx.get("this") match {
      case Some((root, thisLens)) =>
        for {
          mapped <- thisLens.map(path, fn, root.asInstanceOf[thisLens.ThisT], outerCtx = ctx)  // <-- pass ctx here
        } yield ctx.updated("this", (mapped, thisLens))
      case None =>
        ZIO.fail(DynaLensError("Missing 'this' in context for map operation"))
    }

case class IfStmt(
                   condition: Fn[Boolean],
                   thenBlock: Statement,
                   elseBlock: Option[Statement] = None
                 ) extends Statement {

  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Map[String, (Any, DynaLens[?])]] =
    for {
      cond <- condition.resolve(ctx)
      resultCtx <- if cond then
        thenBlock.resolve(ctx)
      else
        elseBlock.map(_.resolve(ctx)).getOrElse(ZIO.succeed(ctx))
    } yield resultCtx
}

case class BlockStmt(statements: List[Statement]) extends Statement:
  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Map[String, (Any, DynaLens[?])]] =
    statements.foldLeft(ZIO.succeed(ctx)) {
      (acc, stmt) => acc.flatMap(stmt.resolve)
    }


case class UpdateStmt[R](
                               path: String,
                               valueFn: Fn[R]
                             ) extends Statement:

  def resolve(ctx: Map[String, (Any, DynaLens[?])]): ZIO[Any, DynaLensError, Map[String, (Any, DynaLens[?])]] =
    parsePath(path) match
      case Nil =>
        ZIO.fail(DynaLensError("update requires a path"))

      case pathHead :: rest if ctx.contains(pathHead.name) =>
        ctx(pathHead.name) match
          case (obj, null) =>
            ZIO.fail(DynaLensError(s"Cannot update val '${pathHead.name}' (no lens)"))
          case (obj, dyanlens) =>
            for {
              value <- valueFn.resolve(ctx)
              updatedObj <- dyanlens
                .asInstanceOf[DynaLens[Any]]
                .update(partialPath(rest), value, obj.asInstanceOf[dyanlens.ThisT])
            } yield ctx.updated(pathHead.name, (updatedObj, dyanlens))

      case _ =>
        ctx.get("this") match
          case Some((obj, dynalens)) =>
            for {
              value <- valueFn.resolve(ctx)
              updatedObj <- dynalens
                .asInstanceOf[DynaLens[Any]]
                .update(path, value, obj.asInstanceOf[dynalens.ThisT])
            } yield ctx.updated("this", (updatedObj, dynalens))
          case None =>
            ZIO.fail(DynaLensError(s"Unable to update: no 'this' context found for path $path"))


/*

Case:  If statement in top-level block (main program)

   if foo then blah  // statement--updates ctx

Case: If fn

   val foo = if blah then bar else wow  // returns value

Case: In map fn if last (or only) statement

   if foo then bar else blah   // if fn

Case: In map fn if NOT last (or only) statement (eg inside a BlockFn given to map)

   if foo then bar else blah   // if statement

*/