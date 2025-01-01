// Copyright: 2017 - 2025 Sam Halliday
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

import scala._

import scalaz.~>

/**
 * Methods to create [[scalaz.NaturalTransformation]] instances for [[CopK]]
 * coproducts
 */
object CopKNaturalTransformation {

  /**
   * Creates a [[scalaz.NaturalTransformation]] from `F` to `G` by fanning in
   * respective NaturalTransformations for type all type constructors
   * in the coproduct `F`.
   *
   * The respective NaturalTransformations are pulled from the input
   * `args` on an as-needed basis; superfluous arguments are ignored.
   */
  def of[F[a] <: CopK[_, a], G[_]](args: Any*): F ~> G =
    macro internal.CopKFunctionKMacros.of[F, G]

  /**
   * Creates a [[scalaz.NaturalTransformation]] from `F` to `G` by fanning in
   * respective NaturalTransformations for type all type constructors
   * in the coproduct `F`.
   *
   * The respective NaturalTransformations are summoned implicitly on
   * an an as-needed basis.
   */
  def summon[F[a] <: CopK[_, a], G[_]]: F ~> G =
    macro internal.CopKFunctionKMacros.summon[F, G]
}
