// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package iotaz

import scala._
import scala.reflect.macros.blackbox

final class IotaMacros(val c: blackbox.Context) {
  import c.universe._

  def prodGen[A, R <: iotaz.TList](
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

      val fromParts = accessors.map(method => q"a.${method.name}")
      val toParts = (accessors.zipWithIndex).map {
        case (method, i) =>
          q"p.values($i).asInstanceOf[${method.typeSignatureIn(A).resultType}]"
      }

      q"""
       _root_.scalaz.Isomorphism.IsoSet[$A, $Prod[$R]](
         (a: $A) => ${Prod.companion}[$R](..$fromParts),
         (p: $Prod[$R]) => ${aSym.companion}(..$toParts): $A
       )
       """
    } else
      c.abort(c.enclosingPosition, "macro only works for classes")
  }

  def copGen[A, R <: iotaz.TList](
    implicit
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
      val provided   = deconstructTCons(R.dealias)
      val calculated = subs.map(_.toTypeConstructor)
      provided.zip(calculated).map {
        case (r, s) =>
          if (!(r =:= s))
            c.abort(
              c.enclosingPosition,
              s"sealed trait parameters are out of order: provided $provided does not match calculated $calculated at $r =:= $s"
            )
      }

      // should probably generate an Inject instead of like this
      val fromParts = subs.zipWithIndex.map {
        case (sub, i) =>
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
