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

package scalaz.iotaz
package internal

import scala._, Predef._
import evidence._

import scalaz._
import Scalaz._

import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.TypecheckException

final class EvidenceMacros(val c: Context) {
  import c.universe._

  private[this] val tb = IotaMacroToolbelt(c)

  def materializeAll[L <: TList](
    implicit
    evL: c.WeakTypeTag[L]
  ): c.Expr[All[L]] = {

    val L = evL.tpe

    tb.foldAbort(for {
      tpes <- tb.memoizedTListTypes(L).leftMap(NonEmptyList.one)
      evs <- tpes
              .traverse(summonEvidence(_).toAvowal.leftMap(NonEmptyList.one))
              .toEither
    } yield q"new ${tb.iotaPackage}.evidence.All[$L](${makeProd(L, evs)})")
  }

  def materializeFirstK[L <: TListK, A](
    implicit
    evL: c.WeakTypeTag[L],
    evA: c.WeakTypeTag[A]
  ): c.Expr[FirstK[L, A]] = {

    val L = evL.tpe
    val A = evA.tpe

    type Acc = Either[List[String], (Type, Int, Tree)]

    tb.foldAbort(for {
      tpes <- tb.memoizedTListKTypes(L).leftMap(List(_))
      tup3 <- tpes.foldLeft(Left(Nil): Acc)((acc, F) =>
               acc match {
                 case Left(e) =>
                   summonEvidence(appliedType(F, A))
                     .leftMap(_ :: e)
                     .map(fa => (F, e.length, fa))
                 case other => other
               }
             )
      (_F, index, fa) = tup3
    } yield q"new ${tb.iotaPackage}.evidence.FirstK[$L, $A](${makeCopK(L, _F, A, index, fa)})")
  }

  def materializeFirstH[L <: TListH, F[_]](
    implicit
    evL: c.WeakTypeTag[L],
    evF: c.WeakTypeTag[F[Nothing]]
  ): c.Expr[FirstH[L, F]] = {

    val L = evL.tpe
    val F = evF.tpe

    type Acc = Either[List[String], (Type, Int, Tree)]

    tb.foldAbort(for {
      tpes <- tb.memoizedTListHTypes(L).leftMap(List(_))
      tup3 <- tpes.foldLeft(Left(Nil): Acc)((acc, H) =>
               acc match {
                 case Left(e) =>
                   summonEvidence(appliedType(H, F))
                     .leftMap(_ :: e)
                     .map(hf => (H, e.length, hf))
                 case other => other
               }
             )
      (_H, index, hf) = tup3
    } yield q"new ${tb.iotaPackage}.evidence.FirstH[$L, $F](${makeCopH(L, _H, F, index, hf)})")
  }

  private[this] def makeProd(
    L: Type,
    values: List[Tree]
  ): Tree =
    q"${tb.iotaPackage}.Prod.unsafeApply[$L]($values)"

  private[this] def makeCopK(
    L: Type,
    F: Type,
    A: Type,
    index: Int,
    fa: Tree
  ): Tree =
    q"${tb.iotaPackage}.CopK.unsafeApply[$L, $F, $A]($index, $fa)"

  private[this] def makeCopH(
    L: Type,
    H: Type,
    F: Type,
    index: Int,
    hf: Tree
  ): Tree =
    q"${tb.iotaPackage}.CopH.unsafeApply[$L, $H, $F]($index, $hf)"

  private[this] def summonEvidence(T: Type): Either[String, Tree] =
    Avowal
      .catching[TypecheckException](
        c.typecheck(q"_root_.scala.Predef.implicitly[$T]")
      )
      .leftMap(_.msg)
      .toEither

}
