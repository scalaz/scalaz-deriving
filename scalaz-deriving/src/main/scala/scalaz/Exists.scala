// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.{ Option, Some }

import Scalaz._

/**
 * Instances of two type constructors for an existential type.
 *
 * Notation is a blend of conjunction `/\` and natural transformation `~>`.
 */
sealed abstract class /~\[A[_], B[_]] {
  type T
  def a: A[T]
  def b: B[T]
}
object /~\ {
  type Exists[A[_], B[_]] = A /~\ B
  type Aux[A[_], B[_], Z] = /~\[A, B] { type T = Z }

  def unapply[A[_], B[_]](p: A /~\ B): Option[(A[p.T], B[p.T])] =
    Some((p.a, p.b))

  type T2[Z] = (Z, Z)
  type L1[Z] = (String, Z)
  type L2[Z] = (String, Z, Z)

  def apply[A[_], B[_], Z](az: =>A[Z], bz: =>B[Z]): Aux[A, B, Z] =
    new /~\[A, B] {
      type T = Z
      def a: A[Z] = az
      def b: B[Z] = bz
    }
}

/**
 * A class of functions from `Z` to a `G[F /~\ ?]` where `?` mirrors the arity
 * of the input parameters.
 */
abstract class ArityExists[Z, F[_], G[_]] {
  import /~\.T2
  def apply(z: Z): G[F /~\ Id]
  def apply(z1: Z, z2: Z): G[F /~\ T2]
}

/** ArityExists with labels */
abstract class LabelledArityExists[Z, F[_], G[_]] {
  import /~\.{ L1, L2 }
  def apply(z: Z): G[F /~\ L1]
  def apply(z1: Z, z2: Z): G[F /~\ L2]
}

/** ArityExists but with special cased arity 1. */
abstract class ArityExists1[Z, F[_], G[_]] {
  import /~\.T2
  def apply(z: Z): F /~\ Id
  def apply(z1: Z, z2: Z): G[F /~\ T2]
}

/** ArityExists1 with Labels. */
abstract class LabelledArityExists1[Z, F[_], G[_]] {
  import /~\.{ L1, L2 }
  def apply(z: Z): F /~\ L1
  def apply(z1: Z, z2: Z): G[F /~\ L2]
}
