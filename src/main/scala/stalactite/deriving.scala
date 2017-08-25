// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalactite

import java.lang.String

import scala.{ Any, AnyRef, Boolean, None, Option, Some, StringContext }
import scala.Predef.{ wrapRefArray, ArrowAssoc }
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

  case class AnyValDesc(name: TypeName, accessor: TermName, tpe: Tree)

  def debug(t: Tree) =
    scala.Predef.println(showRaw(t))
  //scala.Predef.println(showRaw(t, printPositions = true))

  def reposition[T <: Tree](pos: Position)(t: T): T = {
    val symtab =
      c.universe.asInstanceOf[_root_.scala.reflect.internal.SymbolTable]

    object Unposition extends Traverser {
      override def traverse(tree: c.universe.Tree): scala.Unit =
        if (tree.canHaveAttrs) {
          tree.asInstanceOf[symtab.Tree].setPos(symtab.NoPosition)
          super.traverse(tree)
        }
    }

    atPos(pos)(Unposition(t))
  }

  def generateImplicits(annottees: c.Expr[Any]*): c.Expr[Any] = {
    // c.typecheck provides Symbol on the input Tree
    val Apply(Select(_, _), parameters) = c.typecheck(c.prefix.tree)
    // gets the juicy typed bits
    val typeclasses = parameters.map(_.symbol.asModule)

    def toSelect(parts: List[TermName]): Tree = parts match {
      case Nil          => Ident(termNames.ROOTPKG)
      case head :: tail => Select(toSelect(tail), head)
    }

    def parseToTermTree(s: String): Tree =
      toSelect(s.split("[.]").toList.map(TermName(_)).reverse)

    def nameToTypeName(t: Tree): Tree = t match {
      case Ident(name)        => Ident(name.toTypeName)
      case Select(qual, name) => Select(qual, name.toTypeName)
    }

    def toTree(s: Symbol): Tree =
      if (s == NoSymbol || s.name.toString == "<root>")
        Ident(termNames.ROOTPKG)
      else Select(toTree(s.owner), s.name.toTermName)

    // long-winded way of saying
    //
    // implicitly[TC[A]].xmap(new A(_), _.value)
    def genAnyValXmap(t: ModuleSymbol, value: AnyValDesc) = {
      import Flag._
      val typeCons = nameToTypeName(toTree(t))
      Apply(
        Select(
          TypeApply(
            Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")),
                          TermName("Predef")),
                   TermName("implicitly")),
            List(AppliedTypeTree(typeCons, List(value.tpe)))
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

    def toGen(t: ModuleSymbol, anyVal: Option[AnyValDesc]): Tree =
      if (isIde) {
        Literal(Constant(null))
      } else {
        anyVal match {
          case Some(value) => genAnyValXmap(t, value)
          case None =>
            custom.get(t.fullName) match {
              case None =>
                toTree(t) match {
                  case Ident(name) =>
                    Select(Ident(TermName(s"Derived$name")), TermName("gen"))
                  case Select(qual, name) =>
                    Select(Select(qual, TermName(s"Derived$name")),
                           TermName("gen"))
                }
              case Some(other) => parseToTermTree(other)
            }
        }
      }

    def anyVal(c: ClassDef): Option[AnyValDesc] =
      c.impl.parents.flatMap {
        case Ident(name) if name.toString == "AnyVal"        => Some(c)
        case Select(qual, name) if name.toString == "AnyVal" => Some(c)
        case _                                               => None
      }.headOption.flatMap { anyval =>
        anyval.impl.body.collect {
          case ValDef(_, name, tpt, _) =>
            AnyValDesc(anyval.name, name, nameToTypeName(tpt))
        }.headOption
      }

    def update(clazz: Option[ClassDef], comp: ModuleDef): c.Expr[Any] = {
      val q"$mods object $name extends ..$bases { ..$body }" = comp
      val implicits =
        typeclasses.map { tc =>
          val termName = TermName(tc.fullName).encodedName.toTermName
          val typeCons = nameToTypeName(toTree(tc))

          clazz match {
            case None =>
              ValDef(
                Modifiers(Flag.IMPLICIT),
                termName,
                AppliedTypeTree(
                  typeCons,
                  List(SingletonTypeTree(Ident(name.toTermName)))
                ),
                toGen(tc, None)
              )
            case Some(c) =>
              c.tparams match {
                case Nil =>
                  ValDef(
                    Modifiers(Flag.IMPLICIT),
                    termName,
                    AppliedTypeTree(typeCons, List(Ident(c.name))),
                    toGen(tc, anyVal(c))
                  )

                case tparams =>
                  val implicits =
                    if (isIde) Nil
                    else
                      List(
                        tparams.zipWithIndex.map {
                          case (t, i) =>
                            ValDef(
                              Modifiers(Flag.IMPLICIT | Flag.PARAM),
                              TermName(s"evidence$$$i"),
                              AppliedTypeTree(typeCons, List(Ident(t.name))),
                              EmptyTree
                            )
                        }
                      )

                  DefDef(
                    Modifiers(Flag.IMPLICIT),
                    termName,
                    tparams,
                    implicits,
                    AppliedTypeTree(
                      typeCons,
                      List(
                        AppliedTypeTree(
                          Ident(c.name),
                          tparams.map(tp => Ident(tp.name))
                        )
                      )
                    ),
                    toGen(tc, anyVal(c))
                  )
              }
          }
        }

      val replacement = q"""${clazz.getOrElse(EmptyTree)}
            $mods object $name extends ..$bases {
              ..$body
              ..$implicits
            }"""

      c.Expr(reposition(comp.pos)(replacement))
    }

    annottees.map(_.tree) match {
      case (data: ClassDef) :: Nil =>
        val mods =
          if (data.mods.hasFlag(Flag.PRIVATE))
            Modifiers(Flag.PRIVATE, data.mods.privateWithin)
          else if (data.mods.hasFlag(Flag.PROTECTED))
            Modifiers(Flag.PROTECTED, data.mods.privateWithin)
          else NoMods

        val companion =
          atPos(data.pos)(
            ModuleDef(
              mods,
              data.name.toTermName,
              Template(Nil, noSelfType, Nil)
            )
          )
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
