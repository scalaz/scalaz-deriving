// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.Predef.ArrowAssoc
import scala.reflect.macros.whitebox.Context

abstract class DerivingCommon extends BackCompat {
  val c: Context
  import c.universe._

  protected def isIde: Boolean =
    c.universe.isInstanceOf[scala.tools.nsc.interactive.Global]

  protected def debug(t: Tree) =
    scala.Predef.println(showRaw(t))
  //scala.Predef.println(showRaw(t, printPositions = true))

  protected def getParam(key: String): Option[String] =
    c.settings.find(_.startsWith(s"$key=")).map(_.substring(key.length + 1))

  protected def readConfig(): DerivingConfig =
    DerivingConfig
      .targets(getParam("deriving"))
      .map(DerivingConfig(_))
      .fold(
        error => {
          c.error(c.prefix.tree.pos, s"Failed to parse deriving config: $error")
          throw new IllegalStateException
        },
        success => success
      )

  // some classes that add type hints around what a Tree contains
  protected case class TreeTypeName(tree: Tree) {
    def toTermName: TreeTermName =
      TreeTermName(tree match {
        case Ident(name)        => Ident(name.toTermName)
        case Select(qual, name) => Select(qual, name.toTermName)
      })
  }
  protected case class TreeTermName(tree: Tree) {
    def toTypeName: TreeTypeName =
      TreeTypeName(tree match {
        case Ident(name)        => Ident(name.toTypeName)
        case Select(qual, name) => Select(qual, name.toTypeName)
      })
  }
  protected case class TermAndType(term: TreeTermName, cons: TreeTypeName)
  protected object TermAndType {
    def apply(s: ModuleSymbol): TermAndType = {
      val term = TreeTermName(c.internal.gen.mkAttributedStableRef(s))
      TermAndType(term, term.toTypeName)
    }
  }

  protected def createCompanion(data: ClassDef): ModuleDef = {
    val mods =
      if (data.mods.hasFlag(Flag.PRIVATE))
        Modifiers(Flag.PRIVATE, data.mods.privateWithin)
      else if (data.mods.hasFlag(Flag.PROTECTED))
        Modifiers(Flag.PROTECTED, data.mods.privateWithin)
      else NoMods

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
  }

  // typechecks the annotation and resolves the typeclasses associated to the
  // companions referenced there. Keyed by the typeclass fqn.
  protected def findTypeclasses(): List[(String, TermAndType)] = {
    // c.typecheck provides Symbol on the input Tree
    val Apply(Select(_, _), parameters) = c.typecheck(c.prefix.tree)
    // gets the juicy typed bits
    val typeclasses =
      parameters.map(_.symbol.info.typeSymbol.companion.companion.asModule)

    typeclasses.map { tc: ModuleSymbol =>
      // ModuleSymbol is very powerful and only available because we
      // typechecked the annotation. Please do not pass it around to
      // the methods beneath or it will not be possible to migrate
      // the to earlier stages in the compile (e.g. as a plugin).
      tc.fullName -> TermAndType(tc)
    }
  }

  protected def regenModule(comp: ModuleDef, extras: List[Tree]): ModuleDef =
    atPos(comp.pos)(
      treeCopy.ModuleDef(
        comp,
        comp.mods,
        comp.name,
        treeCopy.Template(comp.impl,
                          comp.impl.parents,
                          comp.impl.self,
                          comp.impl.body ::: extras)
      )
    )

}
