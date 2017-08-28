// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalactite

import java.lang.String

import scala.{
  Any,
  AnyRef,
  Boolean,
  Either,
  Left,
  None,
  Option,
  Right,
  Some,
  StringContext
}
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
  private implicit class EitherBackCompat[L, R](e: Either[L, R]) {
    def map[RR](f: R => RR): Either[L, RR] = e match {
      case Left(left)   => Left(left)
      case Right(right) => Right(f(right))
    }

    def flatMap[RR](f: R => Either[L, RR]): Either[L, RR] = e match {
      case Left(left)   => Left(left)
      case Right(right) => f(right)
    }
  }

  private type Config = Either[String, Map[String, String]]
  private def config(path: Option[String]): Config =
    for {
      d <- defaults
      u <- path.fold(Right(Map.empty): Config)(user)
    } yield (d ++ u)

  // cached to avoid hitting disk on every use of the macro
  private[this] var cached: Map[String, Config] = Map.empty
  private[this] def user(path: String): Config =
    cached.get(path) match {
      case Some(got) => got
      case None =>
        val calculated = for {
          s <- readFile(path)
          c <- parseConfig(s)
        } yield c
        cached += path -> calculated
        calculated
    }

  private lazy val defaults: Config =
    for {
      s <- readResource("/stalactite.conf")
      c <- parseConfig(s)
    } yield c

  private[this] def parseConfig(config: String): Config =
    try {
      Right(
        config
          .split("\n")
          .toList
          .filterNot(_.isEmpty)
          .map(_.split("=").toList)
          .map {
            case List(from, to) => from -> to
            case other          =>
              // I'd have used Left with traverse, but this is stdlib...
              throw new java.lang.IllegalArgumentException(
                s"expected 2 parts but got ${other.size} in $other"
              )
          }
          .toMap
      )
    } catch {
      case t: java.lang.Throwable =>
        Left(t.getMessage)
    }

  private[this] def readFile(file: String): Either[String, String] =
    readInputStream(new java.io.FileInputStream(file))

  private[this] def readResource(res: String): Either[String, String] =
    readInputStream(getClass.getResourceAsStream(res))

  private[this] def readInputStream(
    is: java.io.InputStream
  ): Either[String, String] =
    try {
      val baos              = new java.io.ByteArrayOutputStream()
      val data              = scala.Array.ofDim[scala.Byte](2048)
      var len: scala.Int    = 0
      def read(): scala.Int = { len = is.read(data); len }
      while (read != -1) {
        baos.write(data, 0, len)
      }
      Right(new String(baos.toByteArray(), "UTF-8"))
    } catch {
      case t: java.lang.Throwable => Left(t.getMessage)
    } finally is.close()

}

class DerivingMacros(val c: Context) {
  import c.universe._

  private def isIde: Boolean =
    c.universe.isInstanceOf[scala.tools.nsc.interactive.Global]

  private def debug(t: Tree) =
    scala.Predef.println(showRaw(t))
  //scala.Predef.println(showRaw(t, printPositions = true))

  private case class AnyValDesc(name: TypeName, accessor: TermName, tpe: Tree)

  private def readConfig() = {
    val custom =
      c.settings.find(_.startsWith("stalactite.config=")).map(_.substring(18))
    DerivingMacros.config(custom)
  }

  private def toSelect(parts: List[TermName]): Tree = parts match {
    case Nil          => Ident(termNames.ROOTPKG)
    case head :: tail => Select(toSelect(tail), head)
  }

  private def parseToTermTree(s: String): Tree =
    toSelect(s.split("[.]").toList.map(TermName(_)).reverse)

  private def nameToTypeName(t: Tree): Tree = t match {
    case Ident(name)        => Ident(name.toTypeName)
    case Select(qual, name) => Select(qual, name.toTypeName)
  }

  private def toTree(s: Symbol): Tree = c.internal.gen.mkAttributedStableRef(s)

  // long-winded way of saying
  //
  // implicitly[TC[A]].xmap(new A(_), _.value)
  private def genAnyValXmap(t: ModuleSymbol, value: AnyValDesc) = {
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

  private def toGen(config: Map[String, String],
                    t: ModuleSymbol,
                    anyVal: Option[AnyValDesc]): Tree =
    if (isIde) {
      Literal(Constant(null))
    } else {
      anyVal match {
        case Some(value) => genAnyValXmap(t, value)
        case None =>
          config.get(t.fullName) match {
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

  private def anyVal(c: ClassDef): Option[AnyValDesc] =
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

  private def update(config: Map[String, String],
                     typeclasses: List[ModuleSymbol],
                     clazz: Option[ClassDef],
                     comp: ModuleDef): c.Expr[Any] = {
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
                List(SingletonTypeTree(Ident(comp.name.toTermName)))
              ),
              toGen(config, tc, None)
            )
          case Some(c) =>
            c.tparams match {
              case Nil =>
                ValDef(
                  Modifiers(Flag.IMPLICIT),
                  termName,
                  AppliedTypeTree(typeCons, List(Ident(c.name))),
                  toGen(config, tc, anyVal(c))
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
                  toGen(config, tc, anyVal(c))
                )
            }
        }
      }

    val module = atPos(comp.pos)(
      treeCopy.ModuleDef(
        comp,
        comp.mods,
        comp.name,
        treeCopy.Template(comp.impl,
                          comp.impl.parents,
                          comp.impl.self,
                          comp.impl.body ::: implicits)
      )
    )

    // if we try to create the AST directly here, we get: "top-level
    // class without companion can only expand either into an
    // eponymous class or into a block consisting in eponymous
    // companions"
    val replacement =
      q"""${clazz.getOrElse(EmptyTree)}
          $module"""

    c.Expr(replacement)
  }

  def generateImplicits(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val config = readConfig().fold(
      error => {
        c.warning(c.prefix.tree.pos,
                  s"Failed to parse stalactite config: $error")
        Map.empty[String, String]
      },
      success => success
    )

    // c.typecheck provides Symbol on the input Tree
    val Apply(Select(_, _), parameters) = c.typecheck(c.prefix.tree)
    // gets the juicy typed bits
    val typeclasses = parameters.map(_.symbol.asModule)

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
            // if we use ModuleDef directly, it doesn't insert the
            // constructor.
            c.internal.reificationSupport.SyntacticObjectDef(
              mods,
              data.name.toTermName,
              Nil,
              Nil,
              noSelfType,
              Nil
            )
          )
        update(config, typeclasses, Some(data), companion)
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
