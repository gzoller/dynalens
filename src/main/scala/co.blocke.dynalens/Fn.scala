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


trait Fn[R]:
  def recv: Fn[Any] // always defined
  def args: List[Fn[Any]] // could be empty, 1, multiple
  def isOptional: Boolean = false
  def methodName: String = this.getClass.getSimpleName.stripSuffix("Fn").decapitalize
  def posStr: String

  def resolve(ctx: DynaContext): ZIO[_BiMapRegistry, DynaLensError, R]

  /** Default: no sub-nodes. Override in composite nodes. */
  def children: List[Fn[?]] = args

  /** Rebuild with new children (in the same order as `children`). */
  def rebuild(kids: List[Fn[?]]): Fn[R]

  /** Walk the tree and patch every `GetFn("this")` with the given receiver. */
  def withReceiver(newRecv: Fn[?]): Fn[R] =
    val kids: List[Fn[Any]] = newRecv.asInstanceOf[Fn[Any]] :: args
    rebuild(kids).asInstanceOf[Fn[R]]


trait UnaryFn[R] extends Fn[R] {
  override def args: List[Fn[Any]] = Nil
  override def recv: Fn[Any] // must be defined in concrete
  override def isOptional: Boolean = recv.isOptional
}

trait BinaryFn[R] extends Fn[R] {
  override def recv: Fn[Any] // left operand
  override def isOptional: Boolean = recv.isOptional || args.head.isOptional
}

// For methods / n-ary you could still do:
trait MethodFn[R] extends Fn[R] {
  override def recv: Fn[Any]
  override def isOptional: Boolean = recv.isOptional || args.exists(_.isOptional)
}

// Marker trait for boolean-returning functions
trait BooleanFn extends Fn[Boolean]
