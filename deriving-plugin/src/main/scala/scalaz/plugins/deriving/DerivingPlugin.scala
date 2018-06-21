// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz.plugins.deriving

import scala.tools.nsc._

class DerivingPlugin(override val global: Global)
    extends AnnotationPlugin(global) {
  override val name: String           = "deriving"
  override val triggers: List[String] = List("deriving", "xderiving")

  import global._

  private val DerivingMacros =
    Select(
      Select(
        Select(Ident(nme.ROOTPKG), TermName("scalaz")),
        TermName("macros")
      ),
      TermName("DerivingMacros")
    )
  def toGen(f: Tree, a: Tree, target: TermName): Tree =
    if (isIde || isScaladoc) Literal(Constant(null))
    else
      TypeApply(
        Select(DerivingMacros.duplicate, target),
        List(f.duplicate, a.duplicate)
      )

  def updateClass(triggered: List[Tree], clazz: ClassDef): ClassDef = clazz
  def updateCompanion(
    triggered: List[Tree],
    clazz: ClassDef,
    companion: ModuleDef
  ): ModuleDef = {
    val extras = triggered.flatMap { ann =>
      val target = annotationName(ann)
      findTypeclasses(ann).map {
        case (gen, typeclass) =>
          if (clazz.tparams.isEmpty)
            genImplicitVal(gen, typeclass, clazz, target)
          else {
            genImplicitDef(gen, typeclass, clazz, target)
          }
      }
    }
    regenModule(companion, extras)
  }

  def updateModule(triggered: List[Tree], module: ModuleDef): ModuleDef = {
    val extras = triggered.flatMap { ann =>
      val target = annotationName(ann)
      findTypeclasses(ann).map {
        case (gen, typeclass) =>
          genObjectImplicitVal(gen, typeclass, module, target)
      }
    }
    regenModule(module, extras)
  }

  // some classes that add type hints around what a Tree contains
  case class TreeTypeName(tree: Tree) {
    def toTermName: TreeTermName =
      TreeTermName(tree match {
        case Ident(name)        => Ident(name.toTermName)
        case Select(qual, name) => Select(qual, name.toTermName)
      })
  }
  case class TreeTermName(tree: Tree) {
    def toTypeName: TreeTypeName =
      TreeTypeName(tree match {
        case Ident(name)        => Ident(name.toTypeName)
        case Select(qual, name) => Select(qual, name.toTypeName)
      })
  }

  def genImplicitVal(
    memberName: TermName,
    typeclass: TreeTypeName,
    c: ClassDef,
    target: TermName
  ): ValDef =
    ValDef(
      Modifiers(Flag.IMPLICIT | Flag.SYNTHETIC),
      memberName,
      AppliedTypeTree(typeclass.tree.duplicate, List(Ident(c.name))),
      toGen(typeclass.tree, Ident(c.name), target)
    )

  def genImplicitDef(
    memberName: TermName,
    typeclass: TreeTypeName,
    c: ClassDef,
    target: TermName
  ): DefDef = {
    val implicits =
      List(
        c.tparams.zipWithIndex.map {
          case (t, i) =>
            ValDef(
              Modifiers(Flag.IMPLICIT | Flag.PARAM | Flag.SYNTHETIC),
              TermName(s"evidence$$$i"),
              AppliedTypeTree(typeclass.tree.duplicate, List(Ident(t.name))),
              EmptyTree
            )
        }
      )

    val a = AppliedTypeTree(
      Ident(c.name),
      c.tparams.map(tp => Ident(tp.name))
    )

    DefDef(
      Modifiers(Flag.IMPLICIT | Flag.SYNTHETIC),
      memberName,
      c.tparams.map(_.duplicate),
      implicits,
      AppliedTypeTree(typeclass.tree.duplicate, List(a)),
      toGen(typeclass.tree, a, target)
    )
  }

  def genObjectImplicitVal(
    memberName: TermName,
    typeclass: TreeTypeName,
    comp: ModuleDef,
    target: TermName
  ): ValDef = {
    val a = SingletonTypeTree(Ident(comp.name.toTermName))
    ValDef(
      Modifiers(Flag.IMPLICIT | Flag.SYNTHETIC),
      memberName,
      AppliedTypeTree(
        typeclass.tree.duplicate,
        List(a)
      ),
      toGen(typeclass.tree, a, target)
    )
  }

  def findTypeclasses(ann: Tree): List[(TermName, TreeTypeName)] =
    ann.children.collect {
      case s @ Select(_, t) if t != nme.CONSTRUCTOR => TreeTermName(s)
      case i @ Ident(_)                             => TreeTermName(i)
    }.map { ttn =>
      memberName(ttn.tree) -> ttn.toTypeName
    }

  def memberName(t: Tree): TermName = {
    val fqn = "_deriving_" + t.toString.toLowerCase.replace(".", "_")
    TermName(fqn).encodedName.toTermName
  }

  def regenModule(comp: ModuleDef, extras: List[Tree]): ModuleDef =
    treeCopy.ModuleDef(
      comp,
      comp.mods,
      comp.name,
      treeCopy.Template(
        comp.impl,
        comp.impl.parents,
        comp.impl.self,
        comp.impl.body ::: extras.map(_.withAllPos(comp.pos))
      )
    )

}
