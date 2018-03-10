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
    macro XDerivingMacros.gen
}

class XDerivingMacros(override val c: Context) extends DerivingCommon {
  import c.universe._

  private case class AnyValDesc(name: TypeName,
                                accessor: TermName,
                                tpe: TreeTypeName)

  // long-winded way of saying
  //
  // implicitly[TC[A]].xmap(new A(_), _.value)
  private def genAnyValXmap(typeCons: TreeTypeName, value: AnyValDesc) = {
    import Flag._
    Apply(
      Select(
        TypeApply(
          Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")),
                        TermName("Predef")),
                 TermName("implicitly")),
          List(AppliedTypeTree(typeCons.tree, List(value.tpe.tree)))
        ),
        TermName("xmap")
      ),
      List(
        Function(
          List(
            ValDef(Modifiers(PARAM | SYNTHETIC),
                   TermName("x"),
                   TypeTree(),
                   EmptyTree)
          ),
          Apply(Select(New(Ident(value.name)), termNames.CONSTRUCTOR),
                List(Ident(TermName("x"))))
        ),
        Function(List(
                   ValDef(Modifiers(PARAM | SYNTHETIC),
                          TermName("x"),
                          TypeTree(),
                          EmptyTree)
                 ),
                 Select(Ident(TermName("x")), value.accessor))
      )
    )
  }

  private def anyVal(c: ClassDef): Option[AnyValDesc] =
    c.impl.parents.flatMap {
      case Ident(name) if name.toString == "AnyVal"     => Some(c)
      case Select(_, name) if name.toString == "AnyVal" => Some(c)
      case _                                            => None
    }.headOption.flatMap { anyval =>
      anyval.impl.body.collect {
        case ValDef(_, name, tpt, _) =>
          AnyValDesc(anyval.name, name, TreeTypeName(tpt))
      }.headOption
    }

  /**
   * AnyVal is special cased to use an invariant functor
   */
  private def genImplicitVal(
    memberName: TermName,
    typeclass: TermAndType,
    c: ClassDef,
    value: AnyValDesc
  ) =
    ValDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      AppliedTypeTree(typeclass.cons.tree, List(Ident(c.name))),
      genAnyValXmap(typeclass.cons, value)
    )

  private def genImplicitDef(
    memberName: TermName,
    typeclass: TermAndType,
    c: ClassDef,
    tparams: List[TypeDef],
    value: AnyValDesc
  ) = {
    val implicits =
      if (isIde) Nil
      else
        List(
          List(
            ValDef(
              Modifiers(Flag.IMPLICIT | Flag.PARAM),
              TermName(s"ev"),
              AppliedTypeTree(typeclass.cons.tree, List(value.tpe.tree)),
              EmptyTree
            )
          )
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
      genAnyValXmap(typeclass.cons, value)
    )
  }

  private def update(config: DerivingConfig,
                     requested: List[(String, TermAndType)],
                     clazz: ClassDef,
                     comp: ModuleDef,
                     av: AnyValDesc): c.Expr[Any] = {
    val implicits = requested.map {
      case (fqn, typeclass) =>
        val memberName = TermName(fqn).encodedName.toTermName
        val tparams    = clazz.tparams
        if (tparams.isEmpty)
          genImplicitVal(memberName, typeclass, clazz, av)
        else
          genImplicitDef(memberName, typeclass, clazz, tparams, av)
    }

    val module = regenModule(comp, implicits)
    val replacement =
      q"""$clazz
          $module"""

    c.Expr(replacement)
  }

  def gen(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val config      = readConfig()
    val typeclasses = findTypeclasses()

    val trees = annottees.map(_.tree)
    val dca = trees match {
      case (data: ClassDef) :: Nil =>
        anyVal(data).map(av => (data, createCompanion(data), av))
      case (data: ClassDef) :: (companion: ModuleDef) :: Nil =>
        anyVal(data).map(av => (data, companion, av))
      case _ => None
    }
    dca match {
      case Some((data, companion, av)) =>
        update(config, typeclasses, data, companion, av)
      case None =>
        c.abort(
          c.enclosingPosition,
          s"@xderiving can only be applied to AnyVal classes (got $trees)"
        )
    }
  }

}
