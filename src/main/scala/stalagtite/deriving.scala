// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalactite

import java.lang.String

import scala.{ Any, AnyRef, Boolean, None, Option, Some, StringContext }
import scala.Predef.{ ???, wrapRefArray, ArrowAssoc }
import scala.annotation.{ compileTimeOnly, StaticAnnotation }
import scala.collection.immutable.{ ::, List, Map, Nil }
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

@compileTimeOnly("deriving annotation should have been removed")
class deriving(typeclasses: AnyRef*) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro DerivingMacros.generateImplicits
}

object DerivingMacros {
  val lookup: Map[String, String] = Map(
    "play.api.libs.json.Format" -> "play.api.libs.json.Json.format"
  )
}

class DerivingMacros(val c: Context) {
  import c.universe._

  lazy val isIde: Boolean =
    c.universe.isInstanceOf[scala.tools.nsc.interactive.Global]

  lazy val custom: Map[String, String] =
    DerivingMacros.lookup ++ c.settings
      .find(_.startsWith("stalactite="))
      .map { args =>
        args
          .substring(11)
          .split("\\|")
          .toList
          .filterNot(_.isEmpty)
          .map { setting =>
            val List(from, to) = setting.split("=").toList
            (from, to)
          }
          .toMap
      }
      .getOrElse(Map.empty)

  def generateImplicits(annottees: c.Expr[Any]*): c.Expr[Any] = {
    // c.typecheck provides Symbol on the input Tree
    val Apply(Select(_, _), typeclasses) = c.typecheck(c.prefix.tree)

    def toSelectedTypeName(t: Tree): Tree = t match {
      case Ident(name) =>
        val pkg = t.symbol.asTerm.owner.fullName
        Select(c.parse(pkg), name.toTypeName)
      case Select(qual, name) => Select(qual, name.toTypeName)
    }

    def toTermName(t: Tree): TermName =
      TermName(t.symbol.asTerm.fullName).encodedName.toTermName

    def toGen(t: Tree): Tree =
      if (isIde) {
        Literal(Constant(null))
      } else {
        val name = t.symbol.name
        val pkg  = t.symbol.asTerm.owner.fullName
        custom.get(s"$pkg.$name") match {
          case None        => c.parse(s"$pkg.Derived$name.gen")
          case Some(other) => c.parse(other)
        }
      }

    def update(clazz: Option[ClassDef], comp: ModuleDef): c.Expr[Any] = {
      val q"$mods object $name extends ..$bases { ..$body }" = comp

      val implicits =
        typeclasses.map { tc =>
          clazz match {
            case None =>
              ValDef(
                Modifiers(Flag.IMPLICIT),
                toTermName(tc),
                tq"${toSelectedTypeName(tc)}[${name}.type]",
                toGen(tc)
              )
            case Some(c) =>
              c.tparams match {
                case Nil =>
                  ValDef(
                    Modifiers(Flag.IMPLICIT),
                    toTermName(tc),
                    tq"${toSelectedTypeName(tc)}[${c.name}]",
                    toGen(tc)
                  )

                case tparams =>
                  val implicits =
                    if (isIde) Nil
                    else
                      List(
                        tparams.zipWithIndex.map {
                          case (t, i) =>
                            ValDef(
                              Modifiers(
                                Flag.IMPLICIT | Flag.PARAM | Flag.SYNTHETIC
                              ),
                              TermName(s"evidence$$$i"),
                              tq"${toSelectedTypeName(tc)}[${t.name}]",
                              EmptyTree
                            )
                        }
                      )

                  DefDef(
                    Modifiers(Flag.IMPLICIT),
                    toTermName(tc),
                    tparams,
                    implicits,
                    tq"${toSelectedTypeName(tc)}[${c.name}[..${tparams.map(_.name)}]]",
                    toGen(tc)
                  )
              }
          }
        }

      val replacement =
        q"""${clazz.getOrElse(EmptyTree)}
            $mods object $name extends ..$bases {
              ..$body
              ..$implicits
            }"""

      c.Expr(replacement)
    }

    annottees.map(_.tree) match {
      case (data: ClassDef) :: Nil =>
        val p         = data.pos
        val companion = q"object ${data.name.toTermName} {}"
        update(Some(data), companion)
      case (data: ClassDef) :: (companion: ModuleDef) :: Nil =>
        update(Some(data), companion)
      case (obj: ModuleDef) :: Nil =>
        update(None, obj)

      case other :: Nil =>
        c.abort(
          c.enclosingPosition,
          s"@deriving can only be applied to classes and sealed traits (got $other a ${other.getClass})"
        )
    }
  }

}
