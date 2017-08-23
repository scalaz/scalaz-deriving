// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalactite

import java.lang.String

import scala.{ Any, AnyRef, Boolean, None, Some }
import scala.StringContext
import scala.collection.immutable.{ ::, List, Map, Nil }
import scala.Predef.{ ???, wrapRefArray, ArrowAssoc }
import scala.Predef.println

import scala.annotation.{ compileTimeOnly, StaticAnnotation }
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

  // not running inside pcplod at all...
  lazy val isIde: Boolean = false
  //c.compilerSettings.find(_.startsWith("-Ymacro-expand")).isDefined

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
    //java.lang.System.err.println(c.compilerSettings)

    // c.typecheck provides Symbol on the input Tree
    val Apply(Select(_, _), typeclasses) = c.typecheck(c.prefix.tree)

    def toTypeName(t: Tree): Tree = t match {
      case Ident(name) =>
        val pkg = t.symbol.asTerm.owner.fullName
        Select(c.parse(pkg), name.toTypeName)
      case Select(qual, name) => Select(qual, name.toTypeName)
    }

    def toTermName(t: Tree): TermName =
      TermName(t.symbol.asTerm.fullName).encodedName.toTermName

    def toGen(t: Tree): Tree =
      if (isIde) {
        java.lang.System.err.println("INSIDE IDE")
        c.parse("null")
      } else {
        val name = t.symbol.name
        val pkg  = t.symbol.asTerm.owner.fullName
        custom.get(s"$pkg.$name") match {
          case None        => c.parse(s"$pkg.Derived$name.gen")
          case Some(other) => c.parse(other)
        }
      }

    def update(clazz: ClassDef, comp: ModuleDef): c.Expr[Any] = {
      val q"$mods object $name extends ..$bases { ..$body }" = comp

      val vals = typeclasses.map { tc =>
        clazz.tparams match {
          case Nil =>
            ValDef(
              Modifiers(Flag.IMPLICIT),
              toTermName(tc),
              tq"${toTypeName(tc)}[${name.toTypeName}]",
              toGen(tc)
            )

          case tparams =>
            val implicits = tparams.zipWithIndex.map {
              case (t, i) =>
                ValDef(
                  Modifiers(Flag.IMPLICIT | Flag.PARAM | Flag.SYNTHETIC),
                  TermName(s"evidence$$$i"),
                  tq"${toTypeName(tc)}[${t.name.toTypeName}]",
                  EmptyTree
                )
            }

            DefDef(
              Modifiers(Flag.IMPLICIT),
              toTermName(tc),
              tparams,
              List(implicits),
              tq"${toTypeName(tc)}[${name.toTypeName}[..${tparams.map(_.name)}]]",
              toGen(tc)
            )
        }

      }

      c.Expr(q"""$clazz
            $mods object $name extends ..$bases {
              ..$body
              ..$vals
            }""")
    }

    annottees.map(_.tree) match {
      case (data: ClassDef) :: Nil =>
        val companion = q"object ${data.name.toTermName} {}"
        update(data, companion)
      case (data: ClassDef) :: (companion: ModuleDef) :: Nil =>
        update(data, companion)

      case _ =>
        c.abort(
          c.enclosingPosition,
          "@deriving can only be applied to classes and sealed traits"
        )
    }
  }

}
