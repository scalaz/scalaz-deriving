/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

// Derived from https://github.com/frees-io/iota
//
// Copyright (C) 2017-2018 Andy Scott.
// Copyright (c) 2017-2018 47 Degrees. <http://47deg.com>
// All rights reserved.
//
// https://github.com/frees-io/iota/blob/v0.3.10/LICENSE
// https://github.com/frees-io/iota/blob/v0.3.10/NOTICE

package scalaz.iotaz
package internal

import catryoshka.*
import java.lang.StackOverflowError
import scala.*
import scala.Predef.*
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.reflect.api.Universe
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.universe as runtimeUniverse
import scalaz.Applicative
import scalaz.Foldable
import scalaz.Id.Id
import scalaz.Traverse
import scalaz.std.either.*
import scalaz.std.list.*
import scalaz.syntax.foldable.*

private[internal] trait Toolbelt {
  type Uu <: Universe
  val u: Uu
}

private[internal] trait MacroToolbelt extends Toolbelt {
  type Cc <: Context
  val c: Cc

  final override type Uu = c.universe.type
  final override val u: Uu = c.universe

  import u.*

  final val iotaPackage: Tree =
    q"_root_.scalaz.iotaz"
}

object IotaReflectiveToolbelt {
  def apply[U <: Universe](u: U): IotaReflectiveToolbelt[u.type] =
    new IotaReflectiveToolbelt[u.type](u)

  def apply(): IotaReflectiveToolbelt[runtimeUniverse.type] =
    apply(runtimeUniverse)
}

final class IotaReflectiveToolbelt[U <: Universe] private (val u: U)
    extends Toolbelt
    with CoproductAPIs
    with TypeListAPIs
    with TypeListBuilders {
  override type Uu = U
}

private[internal] object IotaMacroToolbelt {
  def apply[C <: Context](c: C): IotaMacroToolbelt[c.type] =
    new IotaMacroToolbelt[c.type](c)

  final class Cache {
    val underlying: TrieMap[Any, Any] = TrieMap.empty[Any, Any]
  }

  final val typeListCache: Cache = new Cache()
}

private[internal] class IotaMacroToolbelt[C <: Context] private (val c: C)
    extends MacroToolbelt
    with CoproductAPIs
    with CoproductMacroAPIs
    with TypeListMacroAPIs
    with TypeListBuilders {
  override type Cc = C
}

// --
// - implementation

private[internal] sealed trait TypeListAST { self: Toolbelt =>
  import u.*

  sealed trait NodeF[+A]
  case class ConsF[A](head: Type, tail: A) extends NodeF[A] with ConsFEquality
  case class ConcatF[A](nodes: List[A]) extends NodeF[A]
  case class ReverseF[A](node: A) extends NodeF[A]
  case class TakeF[A](n: Int, node: A) extends NodeF[A]
  case class DropF[A](n: Int, node: A) extends NodeF[A]
  case class RemoveF[A](t: Type, nodes: A) extends NodeF[A]
  case class MapF[A](f: Type, nodes: A) extends NodeF[A]
  case object NNilF extends NodeF[Nothing]

  sealed trait ConsFEquality { self: ConsF[?] =>
    override def equals(that: Any): Boolean =
      that match {
        case that: ConsF[?] =>
          that.canEqual(this) &&
          this.head =:= that.head && this.tail == that.tail
        case _ => false
      }
  }

  object NodeF {
    implicit val nodeTraverse: Traverse[NodeF] = new Traverse[NodeF] {

      def traverseImpl[G[_], A, B](
        fa: NodeF[A]
      )(f: A => G[B])(implicit G: Applicative[G]): G[NodeF[B]] =
        fa match {
          case ConsF(hd, a) => G.map(f(a))(ConsF(hd, _))
          case ConcatF(as)  => G.map(Traverse[List].traverse(as)(f))(ConcatF(_))
          case ReverseF(a)  => G.map(f(a))(ReverseF(_))
          case TakeF(n, a)  => G.map(f(a))(TakeF(n, _))
          case DropF(n, a)  => G.map(f(a))(DropF(n, _))
          case RemoveF(t, a) => G.map(f(a))(RemoveF(t, _))
          case MapF(ft, a)   => G.map(f(a))(MapF(ft, _))
          case NNilF         => G.pure(NNilF: NodeF[B])
        }

      override def foldLeft[A, B](fa: NodeF[A], b: B)(f: (B, A) => B): B =
        fa match {
          case ConsF(_, a)   => f(b, a)
          case ConcatF(as)   => Foldable[List].foldLeft(as, b)(f)
          case ReverseF(a)   => f(b, a)
          case TakeF(_, a)   => f(b, a)
          case DropF(_, a)   => f(b, a)
          case RemoveF(_, a) => f(b, a)
          case MapF(_, a)    => f(b, a)
          case NNilF         => b
        }
    }
  }
}

private[internal] sealed trait TypeListParsers {
  self: Toolbelt with TypeListAST =>
  import u.*

  type TypeListParser = CoalgebraM[Either[Id[String], *], NodeF, Type]

  final lazy val tlistParser: TypeListParser = typeListParser(
    NilSym = symbolOf[TNil],
    ConsSym = symbolOf[TCons[Nothing, Nothing]],
    ConcatSym = symbolOf[TList.Op.Concat[Nothing, Nothing]],
    ReverseSym = symbolOf[TList.Op.Reverse[Nothing]],
    TakeSym = symbolOf[TList.Op.Take[Nothing, Nothing]],
    DropSym = symbolOf[TList.Op.Drop[Nothing, Nothing]],
    RemoveSym = symbolOf[TList.Op.Remove[Nothing, Nothing]],
    MapSym = symbolOf[TList.Op.Map[Nothing, Nothing]]
  )

  final lazy val tlistkParser: TypeListParser = typeListParser(
    NilSym = symbolOf[TNilK],
    ConsSym = symbolOf[TConsK[Nothing, Nothing]],
    ConcatSym = symbolOf[TListK.Op.Concat[Nothing, Nothing]],
    ReverseSym = symbolOf[TListK.Op.Reverse[Nothing]],
    TakeSym = symbolOf[TListK.Op.Take[Nothing, Nothing]],
    DropSym = symbolOf[TListK.Op.Drop[Nothing, Nothing]],
    RemoveSym = symbolOf[TListK.Op.Remove[Nothing, Nothing]]
  )

  final lazy val tlisthParser: TypeListParser = typeListParser(
    NilSym = symbolOf[TNilH],
    ConsSym = symbolOf[TConsH[Nothing, Nothing]],
    ConcatSym = symbolOf[TListH.Op.Concat[Nothing, Nothing]],
    ReverseSym = symbolOf[TListH.Op.Reverse[Nothing]],
    TakeSym = symbolOf[TListH.Op.Take[Nothing, Nothing]],
    DropSym = symbolOf[TListH.Op.Drop[Nothing, Nothing]],
    RemoveSym = symbolOf[TListH.Op.Remove[Nothing, Nothing]]
  )

  private[this] def symbolOf[T](implicit evT: WeakTypeTag[T]): Symbol =
    evT.tpe.typeSymbol

  private[internal] def typeListParser(
    ConsSym: Symbol,
    NilSym: Symbol,
    ConcatSym: Symbol,
    ReverseSym: Symbol,
    TakeSym: Symbol,
    DropSym: Symbol,
    RemoveSym: Symbol,
    MapSym: Symbol = symbolOf[Disregard]
  ): TypeListParser =
    tpe0 => {
      @tailrec def loop(tpe: Type): Either[Id[String], NodeF[Type]] =
        tpe.dealias match {
          case TypeRef(_, sym, args) =>
            sym.asType.toType.dealias.typeSymbol match {
              case ConsSym    => ConsF(args(0), args(1)).asRight
              case NilSym     => NNilF.asRight
              case ConcatSym  => ConcatF(args).asRight
              case ReverseSym => ReverseF(args(0)).asRight
              case TakeSym    => literalInt(args(0)).map(TakeF(_, args(1)))
              case DropSym    => literalInt(args(0)).map(DropF(_, args(1)))
              case RemoveSym  => RemoveF(args(0), args(1)).asRight
              case MapSym     => MapF(args(0), args(1)).asRight
              case sym        =>
                s"Unexpected symbol $sym for type $tpe: ${showRaw(tpe)}".asLeft
            }
          case ExistentialType(_, res) => loop(res) // the irony...
          case _ => s"Unable to parse type $tpe: ${showRaw(tpe)}".asLeft
        }
      loop(tpe0)
    }

  private[this] def literalInt(tpe: Type): Either[Id[String], Int] =
    tpe.dealias match {
      case ConstantType(Constant(value: Int)) => value.asRight
      case _ => s"Expected $tpe to be a literal integer".asLeft
    }
}

private[internal] sealed trait TypeListEvaluators {
  self: Toolbelt with TypeListAST =>
  import u.*

  type TypeListEvaluator = Algebra[NodeF, List[Type]]

  final def evalTree: TypeListEvaluator = {
    case ConsF(head, types) => head :: types
    case ConcatF(typeLists) => typeLists.flatMap(l => l)
    case ReverseF(types)    => types.reverse
    case TakeF(n, types)    => types.take(n)
    case DropF(n, types)    => types.drop(n)
    case RemoveF(t, types)  => removeFirst(types)(_ =:= t)
    case MapF(tf, types)    => types.map(t => appliedType(tf, t))
    case NNilF              => Nil
  }

  private[this] def removeFirst[T](
    list: List[T]
  )(pred: T => Boolean): List[T] = {
    val (before, atAndAfter) = list.span(!pred(_))
    before ::: atAndAfter.drop(1)
  }
}

private[internal] sealed trait TypeListBuilders {
  self: Toolbelt with TypeListAST =>
  import u.*

  type TypeListBuilder = Algebra[List, Type]

  final lazy val buildTList: TypeListBuilder =
    typeListBuilder(weakTypeOf[TCons[?, ?]].typeConstructor, weakTypeOf[TNil])

  final lazy val buildTListK: TypeListBuilder =
    typeListBuilder(
      weakTypeOf[TConsK[Nothing, ?]].typeConstructor,
      weakTypeOf[TNilK]
    )

  final lazy val buildTListH: TypeListBuilder =
    typeListBuilder(
      weakTypeOf[TConsH[Nothing, ?]].typeConstructor,
      weakTypeOf[TNilH]
    )

  private[internal] def typeListBuilder(
    consTpe: Type,
    nilTpe: Type
  ): TypeListBuilder = {

    lazy val (consPrefix, consSym) = consTpe match {
      case TypeRef(prefix, sym, _) => (prefix, sym)
      case _ => sys.error("internal iota initialization error")
    }

    tpes =>
      tpes.foldRight(nilTpe)((tpe, acc) =>
        internal.typeRef(consPrefix, consSym, tpe :: acc :: Nil)
      )
  }
}

private[internal] sealed trait TypeListAPIs
    extends TypeListAST
    with TypeListParsers
    with TypeListEvaluators {
  self: Toolbelt =>

  import u.*

  final def tlistTypes(tpe: Type): Either[Id[String], List[Type]] =
    hyloM(tpe)(evalTree.generalizeM[Either[Id[String], *]], tlistParser)

  final def tlistkTypes(tpe: Type): Either[Id[String], List[Type]] =
    hyloM(tpe)(evalTree.generalizeM[Either[Id[String], *]], tlistkParser)

  final def tlistkTypeConstructors(tpe: Type): Either[Id[String], List[Type]] =
    tlistkTypes(tpe).map(_.map(_.etaExpand.resultType))

  final def tlisthTypes(tpe: Type): Either[Id[String], List[Type]] =
    hyloM(tpe)(evalTree.generalizeM[Either[Id[String], *]], tlisthParser)

}

private[internal] sealed trait CoproductAPIs { self: Toolbelt =>
  import u.*

  case class CopTypes(L: Type)
  case class CopKTypes(L: Type, A: Type)

  private[this] final lazy val CopTpe =
    typeOf[Cop[Nothing]].etaExpand.resultType
  private[this] final lazy val CopKTpe =
    typeOf[CopK[Nothing, Nothing]].etaExpand.resultType

  private[this] def resultType(sym: Symbol): Type =
    sym.asType.toType.etaExpand.resultType

  final def destructCop(tpe: Type): Either[Id[String], CopTypes] =
    tpe.dealias.resultType match {
      case TypeRef(_, sym, l :: Nil) if resultType(sym) <:< CopTpe =>
        Right(CopTypes(l))
      case TypeRef(_, sym, Nil) => destructCop(sym.asType.toType)
      case t                    =>
        Left(s"unexpected type $t ${showRaw(t)} when destructuring Cop $tpe")
    }

  final def destructCopK(tpe: Type): Either[Id[String], CopKTypes] =
    tpe.dealias.resultType match {
      case TypeRef(_, sym, l :: a :: Nil) if resultType(sym) <:< CopKTpe =>
        Right(CopKTypes(l, a))
      case TypeRef(_, sym, Nil) => destructCopK(sym.asType.toType)
      case t                    =>
        Left(s"unexpected type $t ${showRaw(t)} when destructuring CopK $tpe")
    }

}

private[internal] sealed trait CoproductMacroAPIs { self: MacroToolbelt =>
  import u.*

  /**
   * Converts an eta expanded `PolyType` such as `[z]Either[String, z]`
   * into a type lambda `Tree` `({ type ξ$[z] = Either[String, z] })#ξ$`.
   * The parameter `z` is taken from the original type and used in
   * resulting tree.
   */
  private[this] final def projectPoly(
    tpe: PolyType,
    lambdaName: TypeName = TypeName("ξ$")
  ): Tree =
    SelectFromTypeTree(
      CompoundTypeTree(
        Template(
          q"_root_.scala.AnyRef" :: Nil,
          ValDef(NoMods, termNames.WILDCARD, TypeTree(), EmptyTree),
          TypeDef(
            NoMods,
            lambdaName,
            tpe.typeParams.map(internal.typeDef(_)),
            q"${tpe.resultType}"
          ) :: Nil
        )
      ),
      lambdaName
    )

  /**
   * Converts a `Type` to a `Tree` so that it can be safely
   * lifted into quasiquotes
   */
  private[this] final def toTypeTree(tpe: Type): Tree =
    tpe match {
      case poly: PolyType       => projectPoly(poly)
      case TypeRef(_, sym, Nil) => c.internal.gen.mkAttributedIdent(sym)
      case _ => c.internal.gen.mkAttributedIdent(tpe.typeSymbol)
    }

  private[this] val FastNatTrans =
    tq"$iotaPackage.internal.FastNaturalTransformation"

  final def defineFastFunctionK(
    className: TypeName,
    F: Type,
    G: Type,
    preamble: List[Tree],
    toIndex: TermName => Tree,
    handlers: List[TermName => Tree]
  ): Tree = {

    val A = TypeName("Ξ$")
    val fa = TermName("η$")
    val FF = toTypeTree(F)
    val GG = toTypeTree(G)
    val FA = AppliedTypeTree(FF, Ident(A) :: Nil)
    val GA = AppliedTypeTree(GG, Ident(A) :: Nil)

    val cases = handlers.zipWithIndex.map { case (h, i) => cq"$i => ${h(fa)}" }
    val toStringValue = s"FastFunctionK[$F, $G]<<generated>>"

    q"""
    class $className extends $FastNatTrans[$FF, $GG] {
      ..$preamble
      override def apply[$A]($fa: $FA): $GA =
        (${toIndex(fa)}: @_root_.scala.annotation.switch) match {
          case ..$cases
          case i => throw new _root_.java.lang.Exception(
            "iota internal error: index " + i + " out of bounds for " + this)
        }
      override def toString: _root_.scala.Predef.String = $toStringValue
    }
    """
  }
}

private[internal] sealed trait TypeListMacroAPIs extends TypeListAPIs {
  self: MacroToolbelt =>
  import u.*

  lazy val showAborts: Boolean =
    !c.inferImplicitValue(typeOf[debug.optionTypes.ShowAborts], true).isEmpty

  lazy val showCache: Boolean =
    !c.inferImplicitValue(typeOf[debug.optionTypes.ShowCache], true).isEmpty

  lazy val showTrees: Boolean =
    !c.inferImplicitValue(typeOf[debug.optionTypes.ShowTrees], true).isEmpty

  def foldAbort[F[_]: Foldable, T](
    either: =>Either[F[String], Tree],
    isImplicit: Boolean = false
  ): c.Expr[T] =
    try
      either.fold(
        errors => {
          val error = errors.toList.mkString(", and\n")
          if (isImplicit && showAborts) c.echo(c.enclosingPosition, error)
          c.abort(c.enclosingPosition, error)
        },
        tree => {
          if (showTrees) c.echo(c.enclosingPosition, showCode(tree))
          c.Expr[T](tree)
        }
      )
    catch {
      case e: StackOverflowError =>
        c.echo(
          c.enclosingPosition,
          "iota has burst the stack! please report this error on GitHub"
        )
        throw e
    }

  def memoize[A, B](cache: IotaMacroToolbelt.Cache)(
    a: A,
    f: A => B
  ): B = {
    val b = cache.underlying.getOrElseUpdate(a, f(a)).asInstanceOf[B]
    if (showCache)
      c.echo(c.enclosingPosition, s"ShowCache: $b cached result $b")
    b
  }

  def memoizedTListTypes(tpe: Type): Either[Id[String], List[Type]] =
    memoize(IotaMacroToolbelt.typeListCache)(tpe, tlistTypes)

  def memoizedTListKTypes(tpe: Type): Either[Id[String], List[Type]] =
    memoize(IotaMacroToolbelt.typeListCache)(tpe, tlistkTypes)

  def memoizedTListHTypes(tpe: Type): Either[Id[String], List[Type]] =
    memoize(IotaMacroToolbelt.typeListCache)(tpe, tlisthTypes)
}
