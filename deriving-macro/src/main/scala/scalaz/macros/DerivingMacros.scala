// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz.macros

import scala.Predef._
import scala.reflect.macros.blackbox

final class DerivingMacrosImpl(val c: blackbox.Context) {
  import c.universe._

  def deriving[F[_], A](implicit
    evF: c.WeakTypeTag[F[_]],
    evA: c.WeakTypeTag[A]
  ): Tree = {
    val F   = evF.tpe
    val A   = evA.tpe
    val fqn = F.dealias.typeSymbol.fullName
    readConfig().targets.get(fqn) match {
      case Some(target) => parseTerm(target)
      case None         => q"_root_.scalaz.Deriving.gen[$F, $A]"
    }
  }

  def xderiving[F[_], A](implicit
    evF: c.WeakTypeTag[F[_]],
    evA: c.WeakTypeTag[A]
  ): Tree = {
    val F = evF.tpe
    val A = evA.tpe
    A.decls.collect {
      case m: MethodSymbol if m.isParamAccessor => m.asMethod
    }.toList match {
      case value :: Nil =>
        val hasXmap   = F.decls.find(_.name.toString == "xmap").isDefined
        val access    = value.name
        val U         = value.typeSignatureIn(A).resultType
        def invariant =
          q"""_root_.scalaz.InvariantFunctor[${F.typeSymbol}].xmap(
              _root_.scala.Predef.implicitly[${F.typeSymbol}[$U]],
              (u: $U) => new $A(u),
              (a: $A) => a.$access)"""
        def xmap      =
          q"""_root_.scala.Predef.implicitly[${F.typeSymbol}[$U]].xmap(
              (u: $U) => new $A(u),
              (a: $A) => a.$access)"""
        if (hasXmap) xmap else invariant
      case _            =>
        c.abort(c.enclosingPosition, "only supports classes with one parameter")
    }
  }

  private def readConfig(): DerivingConfig =
    DerivingConfig.targets
      .fold(
        error => {
          c.error(c.prefix.tree.pos, s"Failed to parse deriving config: $error")
          throw new IllegalStateException
        },
        success => DerivingConfig(success)
      )

  private def parseTerm(s: String): Tree = {
    def toSelect(parts: List[TermName]): Tree =
      parts match {
        case Nil          => Ident(termNames.ROOTPKG)
        case head :: tail => Select(toSelect(tail), head)
      }
    toSelect(s.split("[.]").toList.map(TermName(_)).reverse)
  }

}

object DerivingMacros {
  def deriving[F[_], A]: F[A] = macro DerivingMacrosImpl.deriving[F, A]
  def xderiving[F[_], A]: F[A] = macro DerivingMacrosImpl.xderiving[F, A]
}
