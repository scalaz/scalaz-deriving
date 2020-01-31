// Copyright: 2017 - 2020 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

// Derived from https://github.com/frees-io/iota
//
// Copyright (C) 2017-2018 Andy Scott.
// Copyright (c) 2017-2018 47 Degrees. <http://47deg.com>
// All rights reserved.
//
// https://github.com/frees-io/iota/blob/v0.3.10/LICENSE
// https://github.com/frees-io/iota/blob/v0.3.10/NOTICE

package scalaz
package iotaz
package internal

import scala._, Predef._
import scala.annotation.tailrec
import scala.reflect.macros.blackbox.Context

import scalaz.std.list._

final class ProductSeq(p: Product) extends OptimisedIndexedSeq[Any] {
  def apply(i: Int): Any = p.productElement(i)
  def length: Int        = p.productArity
}

final class ArraySeq(p: Array[Any]) extends OptimisedIndexedSeq[Any] {
  def apply(i: Int): Any = p(i)
  def length: Int        = p.length
}

sealed abstract class OptimisedIndexedSeq[A]
    extends collection.immutable.IndexedSeq[A] {

  // optimisations...
  override def toList: List[A] = {
    @tailrec
    def loop(acc: List[A], i: Int): List[A] =
      if (i >= 0) {
        loop(apply(i) :: acc, i - 1)
      } else {
        acc
      }
    loop(Nil, length - 1)
  }
  override def foldRight[B](z: B)(op: (A, B) => B): B = {
    @tailrec
    def loop(acc: B, i: Int): B =
      if (i >= 0) {
        loop(op(apply(i), acc), i - 1)
      } else {
        acc
      }
    loop(z, length - 1)
  }
  override def foldLeft[B](z: B)(op: (B, A) => B): B = {
    val len = length
    @tailrec
    def loop(acc: B, i: Int): B =
      if (i < len) {
        loop(op(acc, apply(i)), i + 1)
      } else {
        acc
      }
    loop(z, 0)
  }
}

private[iotaz] final class ProductMacros(val c: Context) {
  import c.universe._

  private[this] val tb = IotaMacroToolbelt(c)

  def prodApply[L <: TList](args: c.Expr[Any]*)(
    implicit
    evL: c.WeakTypeTag[L]
  ): c.Expr[Prod[L]] = {

    val L = evL.tpe

    val pkg = q"_root_.scalaz.iotaz.internal"

    tb.foldAbort(for {
      algebras <- tb.memoizedTListTypes(L).left.map(NonEmptyList.one(_))
      argTypes = args.toList.map(_.tree.tpe)
      _ <- require(
            argTypes.length == algebras.length,
            s"Expected ${algebras.length} arguments but received ${argTypes.length}"
          )
      _ <- Traverse[List]
            .traverse(argTypes.zip(algebras))(tpes =>
              require(
                tpes._1 <:< tpes._2,
                s"Expected ${tpes._1} <:< ${tpes._2}"
              ).toAvowal
            )
            .toEither
      seq = if (argTypes.length == 0) q"_root_.scala.collection.immutable.Nil"
      // perf testing shows that ArraySeq is faster than ProductSeq
      // for raw fields, but is faster for case classes.
      else q"new $pkg.ArraySeq(_root_.scala.Array[_root_.scala.Any](..$args))"
    } yield q"${tb.iotaPackage}.Prod.unsafeApply[$L]($seq)")
  }

  def prodGen[A, R <: TList](
    implicit
    evA: c.WeakTypeTag[A],
    evR: c.WeakTypeTag[R]
  ): Tree = {
    val A = evA.tpe
    val R = evR.tpe

    val aSym = A.typeSymbol

    val Prod = weakTypeOf[iotaz.Prod[_]].typeSymbol

    if (aSym.isModuleClass) {
      q"""
       _root_.scalaz.Isomorphism.IsoSet[$A, $Prod[$R]](
         (a: $A) => ${Prod.companion}[$R](),
         (p: $Prod[$R]) => ${A.termSymbol}
       )
       """
    } else if (aSym.isClass) {
      val aSym = A.typeSymbol.asClass

      val accessors = A.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      }.toList

      val to =
        if (aSym.isCaseClass)
          q"(a: $A) => ${Prod.companion}.unsafeApply[$R](new _root_.scalaz.iotaz.internal.ProductSeq(a))"
        else {
          val toParts = accessors.map(method => q"a.${method.name}")
          q"(a: $A) => ${Prod.companion}[$R](..$toParts)"
        }

      // inefficient if the underlying is a List...
      val fromParts = (accessors.zipWithIndex).map {
        case (method, i) =>
          q"p.values($i).asInstanceOf[${method.typeSignatureIn(A).resultType}]"
      }
      val from = q"""(p: $Prod[$R]) => ${aSym.companion}(..$fromParts): $A"""

      q"""_root_.scalaz.Isomorphism.IsoSet[$A, $Prod[$R]]($to, $from)"""
    } else
      c.abort(c.enclosingPosition, "macro only works for classes")
  }

  private[this] def require(
    flag: Boolean,
    msg: =>String
  ): Either[NonEmptyList[String], Unit] =
    Either.cond(flag, (), msg).left.map(NonEmptyList.one(_))

}
