// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.annotation.{ compileTimeOnly, StaticAnnotation }
import scala.collection.immutable.{ ::, List, Nil }
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

@compileTimeOnly("xderiving annotation should have been removed")
class xderiving(val typeclasses: AnyRef*) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro XDerivingMacroAnnotation.gen
}

class XDerivingMacroAnnotation(override val c: Context) extends DerivingCommon {
  import c.universe._

  override protected def toGen(f: Tree, a: Tree): Tree =
    if (isIde) Literal(Constant(null))
    else
      TypeApply(
        Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scalaz")),
                      TermName("DerivingMacros")),
               TermName("xderiving")),
        List(f, a)
      )

  private def update(requested: List[(String, TermAndType)],
                     clazz: ClassDef,
                     comp: ModuleDef): c.Expr[Any] = {
    val implicits = requested.map {
      case (fqn, typeclass) =>
        val memberName = TermName(fqn).encodedName.toTermName
        if (clazz.tparams.isEmpty)
          genImplicitVal(memberName, typeclass, clazz)
        else
          genImplicitDef(memberName, typeclass, clazz)
    }

    val module = regenModule(comp, implicits)
    val replacement =
      q"""$clazz
          $module"""

    c.Expr(replacement)
  }

  def gen(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val typeclasses = findTypeclasses()

    annottees.map(_.tree) match {
      case (data: ClassDef) :: Nil =>
        update(typeclasses, data, createCompanion(data))
      case (data: ClassDef) :: (companion: ModuleDef) :: Nil =>
        update(typeclasses, data, companion)
      case _ =>
        c.abort(
          c.enclosingPosition,
          s"@xderiving can only be used on classes with one parameter"
        )
    }
  }

}
