// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz.macros

import scala._
import scala.reflect.macros.blackbox

final class IotaDerivingMacros(val c: blackbox.Context) {
  import c.universe._

  def gen[F[_], A](
    implicit
    evF: c.WeakTypeTag[F[_]],
    evA: c.WeakTypeTag[A]
  ): Tree = {
    val F    = evF.tpe.typeConstructor
    val A    = evA.tpe
    val aSym = A.typeSymbol.asClass

    val TNil   = weakTypeOf[iotaz.TNil]
    val TCons  = weakTypeOf[iotaz.TCons[_, _]].typeConstructor
    val Name   = weakTypeOf[scalaz.Name[_]].typeConstructor
    val String = weakTypeOf[java.lang.String]

    def tlist(parts: List[Type]) =
      parts
        .foldRight(TNil) { (el, els) =>
          appliedType(TCons, el, els)
        }

    val parts = if (aSym.isSealed) {
      // ordering is ill-defined, we use source ordering
      aSym.asClass.knownDirectSubclasses.toList
        .map(_.asClass)
        .sortBy(_.pos.start)
        .map { cl =>
          if (cl.isModuleClass) {
            internal.singleType(cl.thisPrefix, cl.module)
          } else {
            // this block is needed to handle the type parameter on a GADT
            val t = cl.toType
            val args = t.typeArgs.map { a =>
              val sym = a.typeSymbol
              val tSym = A
                .find(_.typeSymbol.name == sym.name)
                .getOrElse(
                  c.abort(
                    c.enclosingPosition,
                    s"type parameters on case classes ($t[${t.typeArgs}]) are not supported unless they are on the sealed trait ($A)"
                  )
                )
              a.substituteTypes(List(sym), List(tSym))
            }
            appliedType(t, args)
          }
        }
    } else {
      A.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor =>
          m.asMethod.typeSignatureIn(A).resultType
      }.toList
    }

    val data   = tlist(parts)
    val tcs    = tlist(parts.map(s => appliedType(Name, appliedType(F, s))))
    val labels = tlist(parts.map(_ => String))

    val tcs_rhs = parts.map { s: Type =>
      val tc = appliedType(F, s)
      val imp =
        c.inferImplicitValue(tc).orElse {
          // when deriving a coproduct, if we can't find implicit evidence for
          // the branches, derive one for use in the coproduct derivation only
          if (aSym.isSealed) {
            // doesn't work when s.typeSymbol.isModuleClass
            // https://gitlab.com/fommil/scalaz-deriving/issues/89
            q"_root_.scalaz.macros.DerivingMacros.deriving[$F, $s]: $tc"
          } else
            // this will fail later on, but with a compiler-generated implicit
            // search failure message (respecting @implicitNotFound &c.)
            // it would be slightly shorter to `inferImplicitValue` unsilently
            // and use whatever message we find in the exception, but exceptions...
            q"_root_.scala.Predef.implicitly[$tc]: $tc"
        }
      q"_root_.scalaz.Need($imp): Name[$tc]"
    }

    if (aSym.isSealed) {
      q"""
       val gen = _root_.iotaz.CopGen.gen[$A, $data, $labels]
       val tcs = _root_.iotaz.Prod[$tcs](..$tcs_rhs)
       _root_.scalaz.Deriving[$F].xcoproductz(tcs, gen.labels, gen.name)(gen.to, gen.from)
       """
    } else {
      q"""
       val gen = _root_.iotaz.ProdGen.gen[$A, $data, $labels]
       val tcs = _root_.iotaz.Prod[$tcs](..$tcs_rhs)
       _root_.scalaz.DerivingProducts[$F].xproductz(tcs, gen.labels, gen.name)(gen.to, gen.from)
       """
    }
  }
}
