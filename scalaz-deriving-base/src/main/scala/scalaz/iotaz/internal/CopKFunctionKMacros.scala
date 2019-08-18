// Copyright: 2017 - 2019 Sam Halliday
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
import scalaz._
import scalaz.std.list._

import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.TypecheckException

final class CopKFunctionKMacros(val c: Context) {
  import c.universe._

  private[this] val tb = IotaMacroToolbelt(c)

  private[this] val NatTransName: String =
    "NaturalTransformation"

  private[this] val NatTransType: Tree =
    tq"_root_.scalaz.NaturalTransformation"

  def of[F[a] <: CopK[_, a], G[_]](args: c.Expr[Any]*)(
    implicit
    evF: c.WeakTypeTag[F[_]],
    evG: c.WeakTypeTag[G[_]]
  ): c.Expr[F ~> G] = {

    val F = evF.tpe
    val G = evG.tpe

    tb.foldAbort(for {
      _ <- guardAssumptions("F", F)
      _ <- guardAssumptions("G", G)

      copK <- tb.destructCopK(F).left.map(NonEmptyList.one(_))
      tpes <- tb.memoizedTListKTypes(copK.L).left.map(NonEmptyList.one(_))

      unorderedPairs <- Traverse[List]
                         .traverse(args.toList)(
                           arg =>
                             destructFunctionKInput(arg.tree.tpe, G)
                               .map((_, arg.tree))
                         )
                         .toEither
      arrs <- Traverse[List]
               .traverse(tpes)(
                 tpe =>
                   unorderedPairs.collectFirst {
                     case (t, arr) if t =:= tpe => arr
                   }.toRight(s"Missing interpreter $NatTransName[$tpe, $G]")
                     .toAvowalNel
               )
               .toEither
    } yield makeInterpreter(F, copK.L, G, arrs))
  }

  def summon[F[a] <: CopK[_, a], G[_]](
    implicit
    evF: c.WeakTypeTag[F[_]],
    evG: c.WeakTypeTag[G[_]]
  ): c.Expr[F ~> G] = {

    val F = evF.tpe
    val G = evG.tpe

    tb.foldAbort(for {
      _ <- guardAssumptions("F", F)
      _ <- guardAssumptions("G", G)

      copK <- tb.destructCopK(F).left.map(NonEmptyList.one(_))
      tpes <- tb.memoizedTListKTypes(copK.L).left.map(NonEmptyList.one(_))

      arrs <- Traverse[List]
               .traverse(tpes)(tpe => summonFunctionK(tpe, G))
               .toEither
    } yield makeInterpreter(F, copK.L, G, arrs))
  }

  private[this] def guardAssumptions(
    name: String,
    T: Type
  ): Either[NonEmptyList[String], _] = T.resultType match {
    case _: ExistentialType =>
      Left(
        NonEmptyList.one(
          s"type parameter $name was inferred to be existential type $T and must be specified"
        )
      )
    case _ if T =:= typeOf[Nothing] =>
      Left(
        NonEmptyList.one(
          s"type parameter $name was inferred to be Nothing and must be specified"
        )
      )
    case _ => Right(())
  }

  private[this] def makeInterpreter(
    F: Type,
    L: Type,
    G: Type,
    arrs: List[Tree]
  ): Tree = {

    val handlers = arrs.zipWithIndex.map {
      case (arr, i) =>
        val name = TermName(s"arr$i")
        val pre =
          q"private[this] val $name = $arr.asInstanceOf[$NatTransType[_root_.scala.Any, $G]]"
        val handler = (fa: TermName) => q"$name($fa.value)"
        (pre, handler)
    }

    val name = TypeName(c.freshName(s"CopK$NatTransName"))
    val defn = tb.defineFastFunctionK(
      name,
      F,
      G,
      preamble = handlers.map(_._1),
      toIndex = fa => q"$fa.index",
      handlers = handlers.map(_._2)
    )

    q"""$defn; new $name"""
  }

  private[this] def summonFunctionK(F: Type, G: Type): AvowalNel[String, Tree] =
    Avowal
      .catching[TypecheckException](
        c.typecheck(q"_root_.scala.Predef.implicitly[$NatTransType[$F, $G]]")
      )
      .leftMap(t => NonEmptyList.one(t.msg))

  private[this] def destructFunctionKInput(
    tpe: Type,
    G: Type
  ): AvowalNel[String, Type] =
    tpe match {
      case TypeRef(_, _, f :: g :: Nil) if g =:= G => Avowal.yes(f)
      case RefinedType(_ :: tpe2 :: Nil, _) =>
        destructFunctionKInput(tpe2.dealias, G)
      case _ =>
        Avowal.noNel(
          s"unable to destruct input $tpe as $NatTransName[?, $G]\n" +
            s"  underlying type tree: ${showRaw { tpe }} (class ${tpe.getClass})"
        )
    }
}
