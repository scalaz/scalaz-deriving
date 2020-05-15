// Copyright: 2017 - 2020 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

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

import scala._

import scalaz.Applicative
import scalaz.Cofree
import scalaz.Functor
import scalaz.Monad
import scalaz.Traverse
import scalaz.syntax.applicative._
import scalaz.syntax.bind._
import scalaz.syntax.traverse._

/** A gross oversimplification/hack of Matryoshka tailored to the
 * specific needs of Iota. This works for both Cats and Scalaz.
 *
 * The vast majoriy of this file is derived/copied/modified from
 * Matryoshka, which is also licensed under the Apache License,
 * version 2.0 (and copyright SlamData Inc).
 *
 * @see https://github.com/slamdata/matryoshka
 */
private[internal] object catryoshka {

  type Algebra[F[_], A]          = F[A] => A
  type AlgebraM[M[_], F[_], A]   = F[A] => M[A]
  type Coalgebra[F[_], A]        = A => F[A]
  type CoalgebraM[M[_], F[_], A] = A => M[F[A]]

  final def hylo[F[_], A, B](
    a: A
  )(alg: Algebra[F, B], coalg: Coalgebra[F, A])(implicit F: Functor[F]): B =
    alg(F.map(coalg(a))(hylo(_)(alg, coalg)))

  final def hyloM[M[_], F[_], A, B](a: A)(
    algM: AlgebraM[M, F, B],
    coalgM: CoalgebraM[M, F, A]
  )(implicit M: Monad[M], F: Traverse[F]): M[B] =
    hylo[λ[α => M[F[α]]], A, M[B]](a)(
      fb => fb >>= (b => b.sequence >>= algM),
      coalgM
    )(M.compose(F))

  trait Based[T] {
    type Base[A]
  }

  trait Recursive[T] extends Based[T] { self =>
    final implicit val recursive: Recursive.Aux[T, Base] = self
    def project(t: T)(implicit BF: Functor[Base]): Base[T]

    def cata[A](t: T)(f: Algebra[Base, A])(implicit BF: Functor[Base]): A =
      hylo(t)(f, project)

    def cataM[M[_]: Monad, A](
      t: T
    )(f: AlgebraM[M, Base, A])(implicit BT: Traverse[Base]): M[A] =
      cata[M[A]](t)(_.sequence >>= f)

    def transCata[U, G[_]: Functor](t: T)(
      f: Base[U] => G[U]
    )(implicit U: Corecursive.Aux[U, G], BF: Functor[Base]): U =
      cata(t)(f.andThen(U.embed(_)))
  }

  object Recursive {
    type Aux[T, F[_]] = Recursive[T] { type Base[A] = F[A] }
    def apply[T](implicit ev: Recursive[T]): Aux[T, ev.Base] = ev
  }

  trait Corecursive[T] extends Based[T] { self =>
    final implicit val corecursive: Corecursive.Aux[T, Base] = self
    def embed(t: Base[T])(implicit BF: Functor[Base]): T

    def ana[A](a: A)(f: Coalgebra[Base, A])(implicit BF: Functor[Base]): T =
      hylo(a)(embed, f)

    def anaM[M[_]: Monad, A](
      a: A
    )(f: CoalgebraM[M, Base, A])(implicit BT: Traverse[Base]): M[T] =
      hyloM[M, Base, A, T](a)(embed(_).pure[M], f)
  }

  object Corecursive {
    type Aux[T, F[_]] = Corecursive[T] { type Base[A] = F[A] }
    def apply[T](implicit ev: Corecursive[T]): Aux[T, ev.Base] = ev
  }

  trait Birecursive[T] extends Recursive[T] with Corecursive[T]

  object Birecursive {
    type Aux[T, F[_]] = Birecursive[T] { type Base[A] = F[A] }
    def apply[T](implicit ev: Birecursive[T]): Aux[T, ev.Base] = ev

    def algebraIso[T, F[_]](
      alg: Algebra[F, T],
      coalg: Coalgebra[F, T]
    ): Birecursive.Aux[T, F] =
      new Birecursive[T] {
        type Base[A] = F[A]
        final def embed(ft: F[T])(implicit F: Functor[F]): T  = alg(ft)
        final def project(t: T)(implicit F: Functor[F]): F[T] = coalg(t)
      }
  }

  sealed trait FixDecl {
    type Fix[F[_]]

    @inline final def apply[F[_]](f: F[Fix.Fix[F]]): Fix[F] = fix(f)

    @inline def fix[F[_]](f: F[Fix.Fix[F]]): Fix[F]
    @inline def unfix[F[_]](f: Fix[F]): F[Fix.Fix[F]]
  }

  type Fix[F[_]] = Fix.Fix[F]
  val Fix: FixDecl = new FixDecl {
    type Fix[F[_]] = F[Fix.Fix[F]]
    def fix[F[_]](f: F[Fix.Fix[F]]): Fix[F]   = f
    def unfix[F[_]](f: Fix[F]): F[Fix.Fix[F]] = f
  }

  implicit def fixBirecursive[F[_]]: Birecursive.Aux[Fix[F], F] =
    Birecursive.algebraIso(Fix.fix, Fix.unfix)

  case class EnvT[B, W[_], A](ask: B, lower: W[A])
  object EnvT {
    implicit def envTTraverse[Z, F[_]](implicit
      F: Traverse[F]
    ): Traverse[EnvT[Z, F, ?]] =
      new Traverse[EnvT[Z, F, ?]] {
        override def foldLeft[A, B](fa: EnvT[Z, F, A], b: B)(
          f: (B, A) => B
        ): B =
          F.foldLeft(fa.lower, b)(f)

        def traverseImpl[G[_], A, B](
          fa: EnvT[Z, F, A]
        )(f: A => G[B])(implicit G: Applicative[G]): G[EnvT[Z, F, B]] =
          G.map(F.traverse(fa.lower)(f))(EnvT(fa.ask, _))
      }
  }

  implicit def cofreeBirecursive[F[_], A]
    : Birecursive.Aux[Cofree[F, A], EnvT[A, F, ?]] =
    Birecursive.algebraIso(
      t => Cofree(t.ask, t.lower),
      t => EnvT(t.head, t.tail)
    )

  implicit final class AlgebraOps[F[_], A](private val self: F[A] => A)
      extends AnyVal {
    // note: above, Algebra[F, A] is expanded to F[A] => A because this
    // apparently has better type inferencing
    def generalizeM[M[_]: Applicative](implicit
      F: Functor[F]
    ): AlgebraM[M, F, A] =
      node => self(node).pure[M]
  }

  implicit final class CoalgebraOps[F[_], A](private val self: Coalgebra[F, A])
      extends AnyVal {
    def assign[B](b: B): Coalgebra[EnvT[B, F, ?], A] =
      a => EnvT[B, F, A](b, self(a))
  }

  implicit final class CoalgebraMOps[M[_], F[_], A](
    private val self: CoalgebraM[M, F, A]
  ) extends AnyVal {
    def assign[B](
      b: B
    )(implicit M: Functor[M]): CoalgebraM[M, EnvT[B, F, ?], A] =
      a => M.map(self(a))(aa => EnvT[B, F, A](b, aa))
  }

}
