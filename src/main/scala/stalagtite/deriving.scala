// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalactite

import java.lang.String

import scala.Any
import scala.StringContext
import scala.collection.immutable.{ ::, List, Map, Nil }
import scala.Predef.{ ???, wrapRefArray }
import scala.Predef.println

import scala.annotation.{ compileTimeOnly, StaticAnnotation }
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

@compileTimeOnly("deriving annotation should have been removed")
class deriving extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro DerivingMacros.generateImplicits
}

class DerivingMacros(val c: Context) {
  import c.universe._

  lazy val custom: Map[String, String] = c.settings
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
    val typeclasses = c.prefix.tree match {
      case q"new deriving(..$b)" => b
    }

    def update(comp: ModuleDef): ModuleDef = {
      val q"$mods object $name extends ..$bases { ..$body }" = comp

      val vals = typeclasses.map { tc =>
        val typ = TypeName(s"$tc[$name]")
        q"""implicit val ${tc}: ${typ} = null"""
      }

      q"""
        $mods object $name extends ..$bases {
          ..$body
          ..$vals
        }
      """
    }

    annottees.map(_.tree) match {
      case (_: ClassDef) :: Nil                              => ???
      case (data: ClassDef) :: (companion: ModuleDef) :: Nil =>
        //println(s"DATA $data\nCOMPANION $companion")

        val updatedCompanion = update(companion)
        println(updatedCompanion)
        c.Expr(q"""$data
                   $updatedCompanion""")

      case _ =>
        c.abort(
          c.enclosingPosition,
          "@deriving can only be applied to classes and sealed traits"
        )
    }
  }

}
