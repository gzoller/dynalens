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

import zio.*
import scala.collection.mutable

// --- Core aliases ------------------------------------------------------------

//
// DynaContext used during runtime execution of compiled scripts
//
// Map[ symbol, (value, lens?) ]
type DynaContext = mutable.Map[String, (Any, Option[DynaLens[?]])]
type Binding     = (Any, Option[DynaLens[?]])

// --- Keys & small helpers ----------------------------------------------------

object CtxKey {
  val Top  = "top"
  val This = "this"
  inline def coll(name: String): String = s"$name[]"

  /** Keys we consider ephemeral (shouldn’t appear in “public” ctx dumps) */
  inline def isEphemeral(k: String): Boolean =
    k == This || k.endsWith("[]")
}

// --- Constructors ------------------------------------------------------------

object DynaContext {
  def apply(target: Any, lens: Option[DynaLens[?]]): DynaContext =
    mutable.Map(CtxKey.Top -> (target, lens))

  def empty: DynaContext = mutable.Map.empty
}

// --- Ergonomic extensions ----------------------------------------------------

extension (ctx: DynaContext)
  /** Immutable-style update (clone + set) when you need to return a new ctx. */
  def updatedWith(k: String, v: Binding): DynaContext = {
    val copy = ctx.clone().asInstanceOf[DynaContext]
    copy += (k -> v)
    copy
  }

  /** Replace or insert a binding (mutable; returns ctx for chaining). */
  def set(k: String, v: Binding): DynaContext = { ctx.update(k, v); ctx }

  /** Remove a key if present (mutable; returns ctx for chaining). */
  def removeKey(k: String): DynaContext = { ctx.remove(k); ctx }

  /** Fetch just the value part if present. */
  def valueOf(k: String): Option[Any] = ctx.get(k).map(_._1)

  /** Public (ephemeral-free) view of entries. */
  def publicEntries: Iterable[(String, Binding)] =
    ctx.iterator.filterNot { case (k, _) => CtxKey.isEphemeral(k) }.toSeq

// --- High-level binders (scoped) --------------------------------------------

/** Generic “with key” bracket: set key, run body, restore previous state. */
def withKeyScoped[R](
                      ctx: DynaContext,
                      key: String,
                      binding: Binding
                    )(
                      body: => ZIO[_BiMapRegistry, DynaLensError, R]
                    ): ZIO[_BiMapRegistry, DynaLensError, R] = {
  val prev: Option[Binding] = ctx.get(key)               // snapshot
  ctx.update(key, binding)                               // set new binding

  // ── IMPORTANT: defend against a null body ────────────────────────────────
  val safeBody: ZIO[_BiMapRegistry, DynaLensError, R] =
    Option(body).getOrElse(
      ZIO.fail(DynaLensError("Internal: null body passed to withKeyScoped"))
    )

  safeBody.ensuring(
    ZIO.succeed {
      prev match {
        case Some(old) => ctx.update(key, old)           // restore
        case None      => ctx.remove(key)                // remove if we introduced it
      }
      ()
    }
  )
}

/** Bind `this` for the duration of `body`. */
def withThisScoped[R](
                       ctx: DynaContext,
                       value: Any,
                       lens: Option[DynaLens[?]] = None
                     )(body: => ZIO[_BiMapRegistry, DynaLensError, R]): ZIO[_BiMapRegistry, DynaLensError, R] =
  withKeyScoped(ctx, CtxKey.This, (value, lens))(body)

/** Bind loop symbol (e.g. “items”) to the current element for the duration of `body`. */
def withLoopSymbol[R](
                       ctx: DynaContext,
                       key: String,
                       value: Any,
                       lens: Option[DynaLens[?]]
                     )(body: => ZIO[_BiMapRegistry, DynaLensError, R]): ZIO[_BiMapRegistry, DynaLensError, R] =
  withKeyScoped(ctx, key, (value, lens))(body)

/** Bind the *whole collection* as `name[]` for the duration of `body`. */
def withCollectionSymbol[R](
                             ctx: DynaContext,
                             loopKey: String,
                             iterable: Iterable[?]
                           )(body: => ZIO[_BiMapRegistry, DynaLensError, R]): ZIO[_BiMapRegistry, DynaLensError, R] =
  withKeyScoped(ctx, CtxKey.coll(loopKey), (iterable, None))(body)

// --- Simple non-scoped convenience (kept for parity with your code) ---------

/** Non-scoped setter for `this` (returns ctx to allow chaining). */
def withElemCtx(elem: Any, ctx: DynaContext): DynaContext =
  ctx.set(CtxKey.This, (elem, None))


// Keep alongside your DynaContext code

object CtxStrings:
  /** Pretty-print ctx. Ephemeral keys (`this`, `name[]`) are hidden by default. */
  def toStringCtx(ctx: DynaContext, includeEphemeral: Boolean = false): String = {
    inline def isEphemeral(k: String): Boolean = k == "this" || k.endsWith("[]")

    // (key, valueOnly) sequence, filtered
    val base: Seq[(String, Any)] =
      ctx.iterator
        .filterNot { case (k, _) => !includeEphemeral && isEphemeral(k) }
        .map { case (k, (v, _)) => (k, v) }
        .toSeq

    // order: "top" first, then alpha
    val ordered =
      base.sortBy { case (k, _) => if (k == "top") "\u0000" else k }

    // match your test snapshots: "key -> value.toString"
    ordered
      .map { case (k, v) => s"$k -> ${Option(v).fold("null")(_.toString)}" }
      .mkString("", "\n", "\n")
  }
