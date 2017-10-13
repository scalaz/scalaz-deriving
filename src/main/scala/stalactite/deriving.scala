// Copyright: 2017 https://gitlab.com/fommil/stalactite/graphs
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

  private type Result[T] = Either[String, T]
  private type Stringy   = Map[String, String]

  type Defaults = List[String]
  private val defaultDefaults: Result[Defaults] = Right(Nil)
  private def defaults(
    path: Option[String]
  ): Result[Defaults] =
    for {
      content <- path.fold(defaultDefaults)(parseOrCachedDefaults)
    } yield content

  private[this] var cachedDefaults: Map[String, Result[Defaults]] = Map.empty
  def parseOrCachedDefaults(path: String): Result[Defaults] =
    cachedDefaults.get(path) match {
      case Some(got) => got
      case None =>
        val calculated: Result[Defaults] = for {
          s <- readFile(path)
          d <- parseDefaults(s)
        } yield d
        cachedDefaults += (path -> calculated)
        calculated
    }
  def parseDefaults(contents: String): Result[Defaults] =
    Right(contents.split("\n").toList.map(_.trim).filterNot(_.isEmpty))

  private val EmptyTargets: Result[Stringy] = Right(Map.empty)
  private def targets(path: Option[String]): Result[Stringy] =
    for {
      d <- defaultTargets
      u <- path.fold(EmptyTargets)(user)
    } yield (d ++ u)

  // cached to avoid hitting disk on every use of the macro
  private[this] var cachedUserTargets: Map[String, Result[Stringy]] = Map.empty
  private[this] def user(path: String): Result[Stringy] =
    cachedUserTargets.get(path) match {
      case Some(got) => got
      case None =>
        val calculated = for {
          s <- readFile(path)
          c <- parseProperties(s)
        } yield c
        cachedUserTargets += path -> calculated
        calculated
    }

  private lazy val defaultTargets: Result[Stringy] =
    for {
      s <- readResource("/stalactite.conf")
      c <- parseProperties(s)
    } yield c

  private[this] def parseProperties(config: String): Result[Stringy] =
    try {
      Right(
        config
          .split("\n")
          .toList
          .filterNot(_.isEmpty)
          .map(_.split("=").toList)
          .map {
            case List(from, to) => from.trim -> to.trim
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
  import DerivingMacros.EitherBackCompat
  import c.universe._

  private def isIde: Boolean =
    c.universe.isInstanceOf[scala.tools.nsc.interactive.Global]

  private def debug(t: Tree) =
    scala.Predef.println(showRaw(t))
  //scala.Predef.println(showRaw(t, printPositions = true))

  // some classes that add type hints around what a Tree contains
  private case class TreeTypeName(tree: Tree) {
    def toTermName: TreeTermName =
      TreeTermName(tree match {
        case Ident(name)        => Ident(name.toTermName)
        case Select(qual, name) => Select(qual, name.toTermName)
      })
  }
  private case class TreeTermName(tree: Tree) {
    def toTypeName: TreeTypeName =
      TreeTypeName(tree match {
        case Ident(name)        => Ident(name.toTypeName)
        case Select(qual, name) => Select(qual, name.toTypeName)
      })
  }
  private case class TermAndType(term: TreeTermName, cons: TreeTypeName)
  private object TermAndType {
    def apply(s: ModuleSymbol): TermAndType = {
      val term = TreeTermName(c.internal.gen.mkAttributedStableRef(s))
      TermAndType(term, term.toTypeName)
    }
  }
  private case class AnyValDesc(name: TypeName,
                                accessor: TermName,
                                tpe: TreeTypeName)

  private case class Config(
    targets: Map[String, String] = Map.empty,
    defaults: List[String] = Nil
  )

  private def getParam(key: String): Option[String] =
    c.settings.find(_.startsWith(s"$key=")).map(_.substring(key.length + 1))

  private def readConfig(): Either[String, Config] =
    for {
      targets  <- DerivingMacros.targets(getParam("stalactite.targets"))
      defaults <- DerivingMacros.defaults(getParam("stalactite.defaults"))
    } yield
      Config(
        targets,
        defaults
      )

  private def parseToTermTree(s: String): TreeTermName = {
    def toSelect(parts: List[TermName]): Tree = parts match {
      case Nil          => Ident(termNames.ROOTPKG)
      case head :: tail => Select(toSelect(tail), head)
    }
    val parts = s.split("[.]").toList.map(TermName(_)).reverse
    TreeTermName(toSelect(parts))
  }

  // long-winded way of saying
  //
  // implicitly[TC[A]].xmap(new A(_), _.value)
  private def genAnyValXmap(typeCons: TreeTypeName, value: AnyValDesc) = {
    import Flag._
    Apply(
      Select(
        TypeApply(
          Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")),
                        TermName("Predef")),
                 TermName("implicitly")),
          List(AppliedTypeTree(typeCons.tree, List(value.tpe.tree)))
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

  private def defaultTarget(term: TreeTermName): TreeTermName =
    TreeTermName(term.tree match {
      case Ident(name) =>
        Select(Ident(TermName(s"Derived$name")), TermName("gen"))
      case Select(qual, name) =>
        Select(Select(qual, TermName(s"Derived$name")), TermName("gen"))
    })

  private def toGen(target: TreeTermName): Tree =
    if (isIde) Literal(Constant(null)) else target.tree

  private def anyVal(c: ClassDef): Option[AnyValDesc] =
    c.impl.parents.flatMap {
      case Ident(name) if name.toString == "AnyVal"        => Some(c)
      case Select(qual, name) if name.toString == "AnyVal" => Some(c)
      case _                                               => None
    }.headOption.flatMap { anyval =>
      anyval.impl.body.collect {
        case ValDef(_, name, tpt, _) =>
          AnyValDesc(anyval.name, name, TreeTypeName(tpt))
      }.headOption
    }

  private def genClassImplicitVal(
    target: TreeTermName,
    memberName: TermName,
    typeclass: TermAndType,
    c: ClassDef
  ) =
    ValDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      AppliedTypeTree(typeclass.cons.tree, List(Ident(c.name))),
      toGen(target)
    )

  private def genClassImplicitDef(
    target: TreeTermName,
    memberName: TermName,
    typeclass: TermAndType,
    c: ClassDef,
    tparams: List[TypeDef]
  ) = {
    val implicits =
      if (isIde) Nil
      else
        List(
          tparams.zipWithIndex.map {
            case (t, i) =>
              ValDef(
                Modifiers(Flag.IMPLICIT | Flag.PARAM),
                TermName(s"evidence$$$i"),
                AppliedTypeTree(typeclass.cons.tree, List(Ident(t.name))),
                EmptyTree
              )
          }
        )

    DefDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      tparams,
      implicits,
      AppliedTypeTree(
        typeclass.cons.tree,
        List(
          AppliedTypeTree(
            Ident(c.name),
            tparams.map(tp => Ident(tp.name))
          )
        )
      ),
      toGen(target)
    )
  }

  private def genObjectImplicitVal(
    target: TreeTermName,
    memberName: TermName,
    typeclass: TermAndType,
    comp: ModuleDef
  ) =
    ValDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      AppliedTypeTree(
        typeclass.cons.tree,
        List(SingletonTypeTree(Ident(comp.name.toTermName)))
      ),
      toGen(target)
    )

  /**
   * The pattern we actually want to generate is more like
   *
   * {{{
   *    val `shapeless.LabelledGeneric` = {
   *      def `shapeless.LabelledGeneric.Aux` = scala.Predef.???
   *      shapeless.LabelledGeneric[Bar]
   *    }
   *    implicit val `shapeless.LabelledGeneric.Aux`
   *      : shapeless.LabelledGeneric.Aux[Bar, `shapeless.LabelledGeneric`.Repr] =
   *      `shapeless.LabelledGeneric`
   * }}}
   *
   * which would expose the actual .Aux. However, generating this
   * results in a compiler error
   *
   * {{{
   * Encountered Valdef without symbol:
   *   implicit val <none>: LabelledGeneric.Aux[Bar, LabelledGeneric.Repr]
   * at UnCurry$UnCurryTransformer.mainTransform(UnCurry.scala:466)
   * }}}
   *
   * which means the type of LabelledGeneric is not being filled in.
   *
   * However, we can do a much simpler alternative which is to
   * generate something like
   *
   * {{{
   *   implicit val `shapeless.LabelledGeneric` = shapeless.LabelledGeneric[Bar]
   * }}}
   *
   * i.e. to put the types on the RHS and let the compiler infer them
   * on the left.
   */
  private def genAuxClassImplicitVal(
    target: TreeTermName,
    memberName: TermName,
    cd: ClassDef
  ): Tree =
    ValDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      TypeTree(),
      TypeApply(
        target.tree,
        List(Ident(cd.name))
      )
    )

  // https://github.com/milessabin/shapeless/issues/757 means that
  // this is rarely useful, since derivation inside yourself (when you
  // are your own companion) is problematic.
  private def genAuxObjectImplicitVal(
    target: TreeTermName,
    memberName: TermName,
    comp: ModuleDef
  ) =
    ValDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      TypeTree(),
      TypeApply(
        target.tree,
        List(SingletonTypeTree(Ident(comp.name.toTermName)))
      )
    )

  // unlike getClassImplicitDef, we do not generate an implicit
  // parameter section (unless this turns out to be required).
  private def genAuxClassImplicitDef(
    target: TreeTermName,
    memberName: TermName,
    c: ClassDef,
    tparams: List[TypeDef]
  ) =
    DefDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      tparams,
      Nil,
      TypeTree(),
      TypeApply(
        target.tree,
        List(
          AppliedTypeTree(
            Ident(c.name),
            tparams.map(tp => Ident(tp.name))
          )
        )
      )
    )

  /**
   * AnyVal is special cased to use an invariant functor
   */
  private def genValueClassImplicitVal(
    target: TreeTermName,
    memberName: TermName,
    typeclass: TermAndType,
    c: ClassDef,
    value: AnyValDesc
  ) =
    ValDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      AppliedTypeTree(typeclass.cons.tree, List(Ident(c.name))),
      genAnyValXmap(typeclass.cons, value)
    )

  private def genValueClassImplicitDef(
    target: TreeTermName,
    memberName: TermName,
    typeclass: TermAndType,
    c: ClassDef,
    tparams: List[TypeDef],
    value: AnyValDesc
  ) = {
    val implicits =
      if (isIde) Nil
      else
        List(
          List(
            ValDef(
              Modifiers(Flag.IMPLICIT | Flag.PARAM),
              TermName(s"ev"),
              AppliedTypeTree(typeclass.cons.tree, List(value.tpe.tree)),
              EmptyTree
            )
          )
        )

    DefDef(
      Modifiers(Flag.IMPLICIT),
      memberName,
      tparams,
      implicits,
      AppliedTypeTree(
        typeclass.cons.tree,
        List(
          AppliedTypeTree(
            Ident(c.name),
            tparams.map(tp => Ident(tp.name))
          )
        )
      ),
      genAnyValXmap(typeclass.cons, value)
    )
  }

  /* typeclass patterns supported */
  private sealed trait Target
  private case class Standard(value: TreeTermName)     extends Target
  private case class LeftInferred(value: TreeTermName) extends Target

  private def update(config: Config,
                     typeclasses: List[ModuleSymbol],
                     clazz: Option[ClassDef],
                     comp: ModuleDef): c.Expr[Any] = {
    val requested = typeclasses.map { tc: ModuleSymbol =>
      // ModuleSymbol is very powerful and only available because we
      // typechecked the annotation. Please do not pass it around to
      // the methods beneath or it will not be possible to migrate
      // the to earlier stages in the compile (e.g. as a plugin).
      tc.fullName -> TermAndType(tc)
    }
    val defaults = config.defaults.map { tc: String =>
      val termName = parseToTermTree(tc)
      tc -> TermAndType(termName, termName.toTypeName)
    }

    // defaults first (typically dependencies of everything else...)
    val implicits = (defaults ++ requested).map {
      case (fqn, typeclass) =>
        val memberName = TermName(fqn).encodedName.toTermName
        val target = config.targets.get(s"$fqn.Aux") match {
          case Some(aux) => LeftInferred(parseToTermTree(aux))
          case None =>
            val to = config.targets
              .get(fqn)
              .map(parseToTermTree)
              .getOrElse(defaultTarget(typeclass.term))
            Standard(to)
        }

        (clazz, target) match {
          case (Some(c), Standard(to)) =>
            (anyVal(c), c.tparams) match {
              case (Some(vt), Nil) =>
                genValueClassImplicitVal(to, memberName, typeclass, c, vt)
              case (Some(vt), tparams) =>
                genValueClassImplicitDef(to,
                                         memberName,
                                         typeclass,
                                         c,
                                         tparams,
                                         vt)
              case (None, Nil) =>
                genClassImplicitVal(to, memberName, typeclass, c)
              case (None, tparams) =>
                genClassImplicitDef(to, memberName, typeclass, c, tparams)
            }
          case (None, Standard(to)) =>
            genObjectImplicitVal(to, memberName, typeclass, comp)

          case (Some(c), LeftInferred(to)) =>
            // LeftInferred is the same for value classes and normal classes
            c.tparams match {
              case _ if isIde => EmptyTree
              case Nil        => genAuxClassImplicitVal(to, memberName, c)
              case tparams    => genAuxClassImplicitDef(to, memberName, c, tparams)
            }
          case (None, LeftInferred(to)) =>
            // see documentation on genAuxObjectImplicitVal
            EmptyTree
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

    //debug(replacement)
    //scala.Predef.println(replacement)

    c.Expr(replacement)
  }

  def generateImplicits(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val config = readConfig().fold(
      error => {
        c.error(c.prefix.tree.pos, s"Failed to parse stalactite config: $error")
        Config()
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
        //debug(companion)
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
