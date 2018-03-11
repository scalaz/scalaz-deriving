// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.annotation.{ compileTimeOnly, StaticAnnotation }
import scala.collection.immutable.{ ::, List, Nil }
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

@compileTimeOnly("deriving annotation should have been removed")
class deriving(val typeclasses: AnyRef*) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro DerivingMacroAnnotation.gen
}

class DerivingMacroAnnotation(override val c: Context) extends DerivingCommon {
  import c.universe._

  override protected def toGen(f: Tree, a: Tree): Tree =
    if (isIde) Literal(Constant(null))
    else
      TypeApply(
        Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scalaz")),
                      TermName("DerivingMacros")),
               TermName("deriving")),
        List(f, a)
      )

  private def update(requested: List[(String, TermAndType)],
                     clazz: Option[ClassDef],
                     comp: ModuleDef): c.Expr[Any] = {
    val implicits = requested.map {
      case (fqn, typeclass) =>
        val memberName = TermName(fqn).encodedName.toTermName

        clazz match {
          case Some(c) =>
            if (c.tparams.isEmpty)
              genImplicitVal(memberName, typeclass, c)
            else
              genImplicitDef(memberName, typeclass, c)
          case None =>
            genObjectImplicitVal(memberName, typeclass, comp)
        }
    }

    val module = regenModule(comp, implicits)

    // if we try to create the AST directly here, we get: "top-level
    // class without companion can only expand either into an
    // eponymous class or into a block consisting in eponymous
    // companions"
    val replacement =
      q"""${clazz.getOrElse(EmptyTree)}
          $module"""

    c.Expr(replacement)
  }

  def gen(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val typeclasses = findTypeclasses()

    annottees.map(_.tree) match {
      case (data: ClassDef) :: Nil =>
        update(typeclasses, Some(data), createCompanion(data))
      case (data: ClassDef) :: (companion: ModuleDef) :: Nil =>
        update(typeclasses, Some(data), companion)
      case (obj: ModuleDef) :: Nil =>
        update(typeclasses, None, obj)
      case other :: Nil =>
        c.abort(
          c.enclosingPosition,
          s"@deriving can only be applied to classes and sealed traits (got $other a ${other.getClass})"
        )
    }
  }

}
