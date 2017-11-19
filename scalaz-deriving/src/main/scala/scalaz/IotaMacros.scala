// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala._
import scala.reflect.macros.blackbox

import iotaz._

final class IotaMacros(val c: blackbox.Context) {
  import c.universe._

  def prodGen[A, R <: TList, L <: TList](
    implicit
    evA: c.WeakTypeTag[A],
    evR: c.WeakTypeTag[R],
    evL: c.WeakTypeTag[L]
  ): Tree = {
    val A = evA.tpe
    val R = evR.tpe
    val L = evL.tpe

    val aSym = A.typeSymbol

    val Prod = weakTypeOf[Prod[_]].typeSymbol

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

  // FIXME: the return type is not checked, e.g. if subtypes are mixed up.
  def copGen[A, R <: TList, L <: TList](
    implicit
    evA: c.WeakTypeTag[A],
    evR: c.WeakTypeTag[R],
    evL: c.WeakTypeTag[L]
  ): Tree = {
    val A = evA.tpe
    val R = evR.tpe
    val L = evL.tpe

    val aSym = A.typeSymbol.asClass

    val Prod = weakTypeOf[Prod[_]].typeSymbol
    val Cop  = weakTypeOf[Cop[_]].typeSymbol

    if (!aSym.isSealed)
      c.abort(c.enclosingPosition, "only supports sealed traits / classes")
    else {
      // ordering is ill-defined
      val subs =
        aSym.asClass.knownDirectSubclasses.toList //.sortBy(_.name.toString)

      // should probably generate an Inject instead of like this
      val fromParts = subs.zipWithIndex.map {
        case (sub, i) =>
          val t = sub.asClass.toType
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
