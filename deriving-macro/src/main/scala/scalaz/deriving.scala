// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.Predef.wrapRefArray
import scala.annotation.{ compileTimeOnly, StaticAnnotation }
import scala.collection.immutable.{ ::, List, Nil }
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

@compileTimeOnly("deriving annotation should have been removed")
class deriving(val typeclasses: AnyRef*) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro DerivingMacros.gen
}

class DerivingMacros(override val c: Context) extends DerivingCommon {
  import c.universe._

  private def parseToTermTree(s: String): TreeTermName = {
    def toSelect(parts: List[TermName]): Tree = parts match {
      case Nil          => Ident(termNames.ROOTPKG)
      case head :: tail => Select(toSelect(tail), head)
    }
    val parts = s.split("[.]").toList.map(TermName(_)).reverse
    TreeTermName(toSelect(parts))
  }

  private def toGen(target: TreeTermName): Tree =
    if (isIde) Literal(Constant(null)) else target.tree

  private def toDerivezGen(
    typeclass: TermAndType,
    clazzTypeTree: Tree
  ): Tree =
    if (isIde) Literal(Constant(null))
    else
      TypeApply(
        Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scalaz")),
                      TermName("Derivez")),
               TermName("gen")),
        List(typeclass.cons.tree, clazzTypeTree)
      )

  private def genDerivezClassImplicitVal(
    memberName: TermName,
    typeclass: TermAndType,
    c: ClassDef
  ) =
    ValDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      AppliedTypeTree(typeclass.cons.tree, List(Ident(c.name))),
      toDerivezGen(typeclass, Ident(c.name))
    )

  private def genDerivezObjectImplicitVal(
    memberName: TermName,
    typeclass: TermAndType,
    comp: ModuleDef
  ) =
    ValDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      AppliedTypeTree(
        typeclass.cons.tree,
        List(SingletonTypeTree(Ident(comp.name.toTermName)))
      ),
      toDerivezGen(typeclass, SingletonTypeTree(Ident(comp.name.toTermName)))
    )

  private def genDerivezClassImplicitDef(
    memberName: TermName,
    typeclass: TermAndType,
    c: ClassDef,
    tparams: List[TypeDef]
  ) = {
    val implicits =
      if (isIde) Nil
      else
        List(
          tparams.zipWithIndex.map {
            case (t, i) =>
              ValDef(
                Modifiers(Flag.IMPLICIT | Flag.PARAM),
                TermName(s"evidence$$$i"),
                AppliedTypeTree(typeclass.cons.tree, List(Ident(t.name))),
                EmptyTree
              )
          }
        )

    DefDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      tparams,
      implicits,
      AppliedTypeTree(
        typeclass.cons.tree,
        List(
          AppliedTypeTree(
            Ident(c.name),
            tparams.map(tp => Ident(tp.name))
          )
        )
      ),
      toDerivezGen(
        typeclass,
        AppliedTypeTree(
          Ident(c.name),
          tparams.map(tp => Ident(tp.name))
        )
      )
    )
  }

  private def genClassImplicitVal(
    target: TreeTermName,
    memberName: TermName,
    typeclass: TermAndType,
    c: ClassDef
  ) =
    ValDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      AppliedTypeTree(typeclass.cons.tree, List(Ident(c.name))),
      toGen(target)
    )

  private def genClassImplicitDef(
    target: TreeTermName,
    memberName: TermName,
    typeclass: TermAndType,
    c: ClassDef,
    tparams: List[TypeDef]
  ) = {
    val implicits =
      if (isIde) Nil
      else
        List(
          tparams.zipWithIndex.map {
            case (t, i) =>
              ValDef(
                Modifiers(Flag.IMPLICIT | Flag.PARAM),
                TermName(s"evidence$$$i"),
                AppliedTypeTree(typeclass.cons.tree, List(Ident(t.name))),
                EmptyTree
              )
          }
        )

    DefDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      tparams,
      implicits,
      AppliedTypeTree(
        typeclass.cons.tree,
        List(
          AppliedTypeTree(
            Ident(c.name),
            tparams.map(tp => Ident(tp.name))
          )
        )
      ),
      toGen(target)
    )
  }

  private def genObjectImplicitVal(
    target: TreeTermName,
    memberName: TermName,
    typeclass: TermAndType,
    comp: ModuleDef
  ) =
    ValDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      AppliedTypeTree(
        typeclass.cons.tree,
        List(SingletonTypeTree(Ident(comp.name.toTermName)))
      ),
      toGen(target)
    )

  /* typeclass patterns supported */
  private sealed trait Target
  private case class Derived(value: TreeTermName) extends Target
  private case object Derivez                     extends Target

  private def update(config: DerivingConfig,
                     requested: List[(String, TermAndType)],
                     clazz: Option[ClassDef],
                     comp: ModuleDef): c.Expr[Any] = {
    val implicits = requested.map {
      case (fqn, typeclass) =>
        val memberName = TermName(fqn).encodedName.toTermName
        val target = config.targets
          .get(fqn)
          .map(parseToTermTree)
          .map(Derived(_))
          .getOrElse(Derivez)

        (clazz, target) match {
          case (Some(c), Derivez) =>
            c.tparams match {
              case Nil =>
                genDerivezClassImplicitVal(memberName, typeclass, c)
              case tparams =>
                genDerivezClassImplicitDef(memberName, typeclass, c, tparams)
            }
          case (None, Derivez) =>
            genDerivezObjectImplicitVal(memberName, typeclass, comp)

          case (Some(c), Derived(to)) =>
            c.tparams match {
              case Nil =>
                genClassImplicitVal(to, memberName, typeclass, c)
              case tparams =>
                genClassImplicitDef(to, memberName, typeclass, c, tparams)
            }
          case (None, Derived(to)) =>
            genObjectImplicitVal(to, memberName, typeclass, comp)
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
    val config      = readConfig()
    val typeclasses = findTypeclasses()

    annottees.map(_.tree) match {
      case (data: ClassDef) :: Nil =>
        update(config, typeclasses, Some(data), createCompanion(data))
      case (data: ClassDef) :: (companion: ModuleDef) :: Nil =>
        update(config, typeclasses, Some(data), companion)
      case (obj: ModuleDef) :: Nil =>
        update(config, typeclasses, None, obj)
      case other :: Nil =>
        c.abort(
          c.enclosingPosition,
          s"@deriving can only be applied to classes and sealed traits (got $other a ${other.getClass})"
        )
    }
  }

}
