// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.Predef.ArrowAssoc
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
  def updateCompanion(triggered: List[Tree],
                      clazz: ClassDef,
                      companion: ModuleDef): ModuleDef
  def updateModule(triggered: List[Tree], module: ModuleDef): ModuleDef

  /** Use to create code that shortcuts in ENSIME and ScalaIDE */
  def isIde = global.isInstanceOf[tools.nsc.interactive.Global]

  // best way to inspect a tree, just call this
  def debug(name: String, tree: Tree): Unit =
    Predef.println(
      s"$name ${tree.id} ${tree.pos}: ${showCode(tree)}\n${showRaw(tree)}"
    )

  // recovers the final part of an annotation
  def annotationName(ann: Tree): TermName =
    ann.children.collectFirst {
      case Select(New(Ident(name)), _)     => name.toTermName
      case Select(New(Select(_, name)), _) => name.toTermName
    }.getOrElse(abort(s"no name for $ann"))

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
    override val phaseName = AnnotationPlugin.this.name
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
          transformer(super.transform(tree))
      }

    val Triggers = triggers.map(newTypeName)
    private def hasTrigger(t: PackageDef): Boolean =
      t.stats.exists {
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

      ModuleDef(
        mods,
        clazz.name.companionName,
        Template(
          List(Select(Ident(nme.scala_), nme.AnyRef.toTypeName)),
          noSelfType,
          List(
            DefDef(
              Modifiers(),
              nme.CONSTRUCTOR,
              Nil,
              List(Nil),
              TypeTree(),
              Block(
                List(
                  Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY),
                               nme.CONSTRUCTOR),
                        Nil)
                ),
                Literal(Constant(()))
              )
            )
          )
        )
      )
    }.withAllPos(clazz.pos)

    def transformer(tree: Tree): Tree = tree match {
      case t: PackageDef if hasTrigger(t) =>
        val classes: Map[TypeName, ClassDef] = t.stats.collect {
          case c: ClassDef => c.name -> c
        }(breakOut)

        val modules: Map[TermName, ModuleDef] = t.stats.collect {
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

        val updated = t.stats.flatMap {
          case ClassNoCompanion(c) if hasTrigger(c.mods) =>
            val companion        = genCompanion(c)
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

        treeCopy.PackageDef(t, t.pid, updated)

      case t => t
    }

  }

  override lazy val components: List[PluginComponent] = List(phase)

}
