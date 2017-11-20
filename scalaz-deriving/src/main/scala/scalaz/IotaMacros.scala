// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala._
import scala.reflect.macros.blackbox

final class IotaMacros(val c: blackbox.Context) {
  import c.universe._

  def prodGen[A, R <: iotaz.TList, L <: iotaz.TList](
    implicit
    evA: c.WeakTypeTag[A],
    evR: c.WeakTypeTag[R],
    evL: c.WeakTypeTag[L]
  ): Tree = {
    val A = evA.tpe
    val R = evR.tpe
    val L = evL.tpe

    val aSym = A.typeSymbol

    val Prod = weakTypeOf[iotaz.Prod[_]].typeSymbol

    if (aSym.isModuleClass) {
      q"""
       _root_.scalaz.ProdGen[$A, $R, $L](
         (a: $A) => ${Prod.companion}[$R](),
         (p: $Prod[$R]) => ${A.termSymbol},
         ${Prod.companion}[$L]()
       )
       """
    } else if (aSym.isClass) {
      val aSym = A.typeSymbol.asClass

      val accessors = A.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      }.toList

      val fromParts = accessors.map(method => q"a.${method.name}")
      val toParts = (accessors zipWithIndex).map {
        case (method, i) =>
          q"p.values($i).asInstanceOf[${method.typeSignatureIn(A).resultType}]"
      }
      val labelParts = accessors.map(method => method.name.toString)

      q"""
       _root_.scalaz.ProdGen[$A, $R, $L](
         a => ${Prod.companion}[$R](..$fromParts),
         p => ${aSym.companion}(..$toParts): $A,
         ${Prod.companion}[$L](..$labelParts)
       )
       """
    } else
      c.abort(c.enclosingPosition, "macro only works for classes")
  }

  def copGen[A, R <: iotaz.TList, L <: iotaz.TList](
    implicit
    evA: c.WeakTypeTag[A],
    evR: c.WeakTypeTag[R],
    evL: c.WeakTypeTag[L]
  ): Tree = {
    val A = evA.tpe
    val R = evR.tpe
    val L = evL.tpe

    val aSym = A.typeSymbol.asClass

    val Prod = weakTypeOf[iotaz.Prod[_]].typeSymbol
    val Cop  = weakTypeOf[iotaz.Cop[_]].typeSymbol

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
      val provided   = deconstructTCons(R)
      val calculated = subs.map(_.toTypeConstructor)
      (provided zip calculated).map {
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
      val labelParts = subs.map(sub => sub.name.toString)

      q"""
        _root_.scalaz.CopGen[$A, $R, $L](
          a => a match { case ..$fromParts },
          c => c.value.asInstanceOf[$A],
          ${Prod.companion}[$L](..$labelParts)
        )
      """
    }

  }

}
