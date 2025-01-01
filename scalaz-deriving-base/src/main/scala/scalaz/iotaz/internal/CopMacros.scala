// Copyright: 2017 - 2025 Sam Halliday
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

import scalaz.iotaz.TList
import scala._
import scala.reflect.macros.blackbox.Context

private[iotaz] final class CopMacros(val c: Context) {
  import c.universe._

  def copGen[A, R <: TList](implicit
    evA: c.WeakTypeTag[A],
    evR: c.WeakTypeTag[R]
  ): Tree = {
    val A = evA.tpe
    val R = evR.tpe

    val aSym = A.typeSymbol.asClass

    val Cop = weakTypeOf[iotaz.Cop[_]].typeSymbol

    if (!aSym.isSealed)
      c.abort(c.enclosingPosition, "only supports sealed traits / classes")
    else {
      // ordering is ill-defined, we use source ordering
      val subs =
        aSym.asClass.knownDirectSubclasses.toList
          .map(_.asClass)
          .sortBy(_.pos.start)

      // we need to reconstruct the TList so the compiler can confirm that the
      // order of subs matches the order in R. Ignores type parameters.

      def deconstructTCons(t: Type): List[Type] = {
        val args = t.typeArgs
        if (args.isEmpty) Nil
        else {
          val List(head, tail) = args
          head.typeConstructor :: deconstructTCons(tail)
        }
      }
      val provided                              = deconstructTCons(R.dealias)
      val calculated                            = subs.map(_.toTypeConstructor)
      provided.zip(calculated).map { case (r, s) =>
        if (!(r =:= s))
          c.abort(
            c.enclosingPosition,
            s"sealed trait parameters are out of order: provided $provided does not match calculated $calculated at $r =:= $s"
          )
      }

      // should probably generate an Inject instead of like this
      val fromParts = subs.zipWithIndex.map { case (sub, i) =>
        val t = sub.toType
        cq"c: $t => ${Cop.companion}.unsafeApply[$R, $t]($i, c)"
      }

      q"""
        _root_.scalaz.Isomorphism.IsoSet[$A, $Cop[$R]](
          (a: $A) => a match { case ..$fromParts },
          (c: $Cop[$R]) => c.value.asInstanceOf[$A]
        )
      """
    }
  }

}
