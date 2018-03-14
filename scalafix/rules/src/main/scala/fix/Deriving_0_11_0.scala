package fix

import scalafix._
import scala.meta._

final case class Deriving_0_11_0(index: SemanticdbIndex)
    extends SemanticRule(index, "Deriving_0_11_0") {

  // can't do Symbol lookup on annotations until
  // https://github.com/scalameta/scalameta/pull/1316
  def hasAnnotation(mods: List[Mod], check: String): Boolean =
    mods.exists {
      case Mod.Annot(Init(Type.Name(`check`), _, _))                 => true
      case Mod.Annot(Init(Type.Select(_, Type.Name(`check`)), _, _)) => true
      // WORKAROUND
      case Mod.Annot(Init(other, _, _)) =>
        false
      case other => false
    }

  def renameAnnotation(mods: List[Mod], from: String, to: String): List[Mod] =
    mods.map {
      case Mod.Annot(Init(Type.Name(`from`), name, args)) =>
        Mod.Annot(Init(Type.Name(to), name, args))
      case Mod.Annot(Init(Type.Select(_, Type.Name(`from`)), name, args)) =>
        // WORKAROUND
        Mod.Annot(Init(Type.Name(to), name, args))
      case other =>
        other
    }

  val AnyVal = Symbol("_root_.scala.AnyVal#")
  // coming soon...
  // val AnyVal = SymbolMatcher.exact("scala.AnyVal#")

  object DerivingAnyVal {
    def unapply(tree: Tree): Option[Defn.Class] = tree match {
      case t @ Defn.Class(
            mods,
            _,
            _,
            Ctor.Primary(_, _, List(List(param))),
            // WORKAROUND dealiasing not supported
            Template(_,
                     List(
                       Init(index.Symbol(AnyVal) | Type.Name("AnyVal"), _, _)),
                     _,
                     _)) if hasAnnotation(mods, "deriving") =>
        Some(t)
      case _ => None
    }
  }

  override def fix(ctx: RuleCtx): Patch = {
    val renames = ctx.tree.collect {
      case DerivingAnyVal(t) =>
        val mods = renameAnnotation(t.mods, "deriving", "xderiving")
        ctx.replaceTree(t, t.copy(mods = mods).syntax)
    }.asPatch

    val imports = {
      if (renames.nonEmpty)
        List(ctx.addGlobalImport(importer"scalaz.xderiving"))
      else
        Nil
    }.asPatch

    renames + imports
  }

}
