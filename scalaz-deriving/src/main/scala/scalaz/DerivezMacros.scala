// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala._
import scala.reflect.macros.blackbox

//import iotaz._

final class DerivezMacros(val c: blackbox.Context) {
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
    } else {
      A.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor =>
          m.asMethod.typeSignatureIn(A).resultType
      }.toList
    }

    val data     = tlist(parts)
    val tcsParts = parts.map(s => appliedType(F, List(s)))
    val tcs      = tlist(tcsParts.map(tc => appliedType(Name, List(tc))))
    val labels   = tlist(parts.map(_ => String))

    val tcs_rhs = tcsParts.map { tc =>
      q"_root_.scalaz.Need(_root_.scala.Predef.implicitly[$tc])"
    }

    if (aSym.isSealed) {
      q"""
       val gen = _root_.scalaz.CopGen.gen[$A, $data, $labels]
       val tcs = _root_.iotaz.Prod[$tcs](..$tcs_rhs)
       _root_.scalaz.Derivez[$F].xcoproductz(tcs)(gen.to, gen.from)
       """
    } else {
      q"""
       val gen = _root_.scalaz.ProdGen.gen[$A, $data, $labels]
       val tcs = _root_.iotaz.Prod[$tcs](..$tcs_rhs)
       _root_.scalaz.Derivez[$F].xproductz(tcs)(gen.to, gen.from)
       """
    }
  }
}
