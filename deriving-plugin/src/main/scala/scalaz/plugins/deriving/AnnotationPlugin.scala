// Copyright: 2017 - 2019 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

// To install: copy this file, changing the package. Canonical latest is
// https://gitlab.com/fommil/scalaz-deriving/tree/master/deriving-plugin
package scalaz.plugins.deriving

import scala.Predef.ArrowAssoc
import scala.deprecated
import scala.collection.immutable.Map
import scala.collection.breakOut
import scala.reflect.internal.util._
import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform._

/**
 * A compiler plugin for code generation when an annotation is on a class, trait
 * or object. For classes and traits, ensures that a companion is created and
 * passes both to the implementation.
 *
 * TL;DR a drop-in replacement for syntactic annotation macros.
 *
 * The main advantage over annotation macros is that it works in all tooling
 * (except IntelliJ) and this approach will work in the long term
 * (macro-paradise is abandoned).
 *
 * The caveats are that typechecking and naming will not occur, so the code
 * generation / rewrites must be purely syntactic. If you require any type
 * inference, point your generated code at a blackbox macro that you implement
 * downstream.
 *
 * Unlike macro annotations, the annotation is not removed. This avoids false
 * negatives regarding unused imports. It should be possible to add a -cleanup
 * phase to remove the trigger annotations if needed.
 */
abstract class AnnotationPlugin(override val global: Global) extends Plugin {

  import global._
  override lazy val description: String =
    s"Generates code for annotations $triggers"

  /** Annotations that trigger the plugin */
  def triggers: List[String]

  def updateClass(triggered: List[Tree], clazz: ClassDef): ClassDef
  def updateCompanion(
    triggered: List[Tree],
    clazz: ClassDef,
    companion: ModuleDef
  ): ModuleDef
  def updateModule(triggered: List[Tree], module: ModuleDef): ModuleDef

  /** Use to create code that shortcuts in ENSIME and ScalaIDE */
  def isIde: Boolean      = global.isInstanceOf[tools.nsc.interactive.Global]
  def isScaladoc: Boolean = global.isInstanceOf[tools.nsc.doc.ScaladocGlobal]

  // best way to inspect a tree, just call this
  def debug(name: String, tree: Tree): Unit =
    Predef.println(
      s"====\n$name ${tree.id} ${tree.pos}:\n${showCode(tree)}\n${showRaw(tree)}"
    )

  // recovers the final part of an annotation
  def annotationName(ann: Tree): TermName =
    ann.children.collectFirst {
      case Select(New(Ident(name)), _)     => name.toTermName
      case Select(New(Select(_, name)), _) => name.toTermName
    }.getOrElse(abort(s"no name for $ann"))

  // case classes without companions should inherit Function
  def addSuperFunction(@deprecated("unused", "") clazz: ClassDef): Boolean =
    true

  implicit class RichTree[T <: Tree](private val t: T) {

    /** when generating a tree, use this to generate positions all the way down. */
    def withAllPos(pos: Position): T = {
      t.foreach { p =>
        val _ = p.setPos(
          new TransparentPosition(pos.source, pos.start, pos.end, pos.end)
        )
      }
      t
    }
  }

  private def phase = new PluginComponent with TypingTransformers {
    override val phaseName: String = AnnotationPlugin.this.name
    override val global: AnnotationPlugin.this.global.type =
      AnnotationPlugin.this.global
    override final def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit =
        newTransformer(unit).transformUnit(unit)
    }
    override val runsAfter: List[String]  = "parser" :: Nil
    override val runsBefore: List[String] = "namer" :: Nil

    private def newTransformer(unit: CompilationUnit) =
      new TypingTransformer(unit) {
        override def transform(tree: Tree): Tree =
          autobots(super.transform(tree))
      }

    val Triggers: List[TypeName] = triggers.map(newTypeName)
    private def hasTrigger(t: Tree): Boolean = t.exists {
      case c: ClassDef if hasTrigger(c.mods)  => true
      case m: ModuleDef if hasTrigger(m.mods) => true
      case _                                  => false
    }

    private def hasTrigger(mods: Modifiers): Boolean =
      Triggers.exists(mods.hasAnnotationNamed)

    private def extractTrigger(c: ClassDef): (ClassDef, List[Tree]) = {
      val trigger = getTriggers(c.mods.annotations)
      //val mods = c.mods.mapAnnotations { anns => anns.filterNot(isNamed(_, Trigger)) }
      // if we remove the annotation, like a macro annotation, we end up with a
      // compiler warning saying that the annotation is unused. Perhaps we could
      // remove it after such warnings are emitted.
      //val update = treeCopy.ClassDef(c, mods, c.name, c.tparams, c.impl)
      val update = c
      (update, trigger)
    }
    private def extractTrigger(m: ModuleDef): (ModuleDef, List[Tree]) = {
      val trigger = getTriggers(m.mods.annotations)
      //val mods = m.mods.mapAnnotations { anns => anns.filterNot(isNamed(_, Trigger)) }
      //val update = treeCopy.ModuleDef(m, mods, m.name, m.impl)
      val update = m
      (update, trigger)
    }

    private def getTriggers(anns: List[Tree]): List[Tree] =
      anns.filter(a => Triggers.exists(isNamed(a, _)))

    private def isNamed(t: Tree, name: TypeName) = t match {
      case Apply(Select(New(Ident(`name`)), _), _)     => true
      case Apply(Select(New(Select(_, `name`)), _), _) => true
      case _                                           => false
    }

    /** generates a zero-functionality companion for a class */
    private def genCompanion(clazz: ClassDef): ModuleDef = {
      val mods =
        if (clazz.mods.hasFlag(Flag.PRIVATE))
          Modifiers(Flag.PRIVATE, clazz.mods.privateWithin)
        else if (clazz.mods.hasFlag(Flag.PROTECTED))
          Modifiers(Flag.PROTECTED, clazz.mods.privateWithin)
        else NoMods

      val isCase = clazz.mods.hasFlag(Flag.CASE)

      lazy val accessors = clazz.impl.collect {
        case ValDef(mods, _, tpt, _) if mods.hasFlag(Flag.CASEACCESSOR) =>
          tpt.duplicate
      }

      def sup =
        if (isCase &&
            clazz.tparams.isEmpty &&
            addSuperFunction(clazz) &&
            accessors.size <= 22) {
          AppliedTypeTree(
            Select(
              Select(Ident(nme.ROOTPKG), nme.scala_),
              TypeName(s"Function${accessors.size}")
            ),
            accessors ::: List(Ident(clazz.name))
          )
        } else
          Select(Ident(nme.scala_), nme.AnyRef.toTypeName)

      def toString_ =
        DefDef(
          Modifiers(Flag.OVERRIDE | Flag.SYNTHETIC),
          nme.toString_,
          Nil,
          Nil,
          Select(Select(Ident(nme.java), nme.lang), nme.String.toTypeName),
          Literal(Constant(clazz.name.companionName.decode))
        )

      val cons = DefDef(
        Modifiers(Flag.SYNTHETIC),
        nme.CONSTRUCTOR,
        Nil,
        List(Nil),
        TypeTree(),
        Block(List(pendingSuperCall), Literal(Constant(())))
      )

      ModuleDef(
        mods | Flag.SYNTHETIC,
        clazz.name.companionName,
        Template(
          List(sup),
          noSelfType,
          cons :: (if (isCase) List(toString_) else Nil)
        )
      )
    }

    // responds to visiting all the parts of the tree and passes to decepticons
    // to do the rewrites
    def autobots(tree: Tree): Tree = tree match {
      case t: PackageDef if hasTrigger(t) =>
        val updated = decepticons(t.stats)
        treeCopy.PackageDef(t, t.pid, updated)

      case t: ModuleDef if hasTrigger(t.impl) =>
        val update = treeCopy.Template(
          t.impl,
          t.impl.parents,
          t.impl.self,
          decepticons(t.impl.body)
        )
        treeCopy.ModuleDef(
          t,
          t.mods,
          t.name,
          update
        )

      case t: ClassDef if hasTrigger(t.impl) =>
        // yuck, this finds classes and modules defined inside other classes.
        // added for completeness.
        val update = treeCopy.Template(
          t.impl,
          t.impl.parents,
          t.impl.self,
          decepticons(t.impl.body)
        )
        treeCopy.ClassDef(
          t,
          t.mods,
          t.name,
          t.tparams,
          update
        )

      case t => t
    }

    // does not recurse, let the autobots handle that
    def decepticons(trees: List[Tree]): List[Tree] = {
      val classes: Map[TypeName, ClassDef] = trees.collect {
        case c: ClassDef => c.name -> c
      }(breakOut)

      val modules: Map[TermName, ModuleDef] = trees.collect {
        case m: ModuleDef => m.name -> m
      }(breakOut)

      object ClassNoCompanion {
        def unapply(t: Tree): Option[ClassDef] = t match {
          case c: ClassDef if !modules.contains(c.name.companionName) =>
            Some(c)
          case _ => None
        }
      }

      object ClassHasCompanion {
        def unapply(t: Tree): Option[ClassDef] = t match {
          case c: ClassDef if modules.contains(c.name.companionName) =>
            Some(c)
          case _ => None
        }
      }

      object CompanionAndClass {
        def unapply(t: Tree): Option[(ModuleDef, ClassDef)] = t match {
          case m: ModuleDef =>
            classes.get(m.name.companionName).map { c =>
              (m, c)
            }
          case _ => None
        }
      }

      trees.flatMap {
        case ClassNoCompanion(c) if hasTrigger(c.mods) =>
          val companion        = genCompanion(c).withAllPos(c.pos)
          val (cleaned, ann)   = extractTrigger(c)
          val updatedCompanion = updateCompanion(ann, cleaned, companion)
          List(updateClass(ann, cleaned), updatedCompanion)

        case ClassHasCompanion(c) if hasTrigger(c.mods) =>
          val (cleaned, ann) = extractTrigger(c)
          List(updateClass(ann, cleaned))

        case CompanionAndClass(companion, c) if hasTrigger(c.mods) =>
          val (cleaned, ann) = extractTrigger(c)
          List(updateCompanion(ann, cleaned, companion))

        case m: ModuleDef if hasTrigger(m.mods) =>
          val (cleaned, ann) = extractTrigger(m)
          List(updateModule(ann, cleaned))

        case tr =>
          List(tr)
      }
    }

  }

  override lazy val components: List[PluginComponent] = List(phase)

}
