// Copyright: 2017 - 2022 Sam Halliday
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
package evidence

import scala.AnyVal

/**
 * Implicit evidence helpers for summoning products and coproducts by
 * summing the constituents.
 *
 * Note: These APIs are new and the organization/naming is subject to
 * change across minor release version numbers. The functionality,
 * however, won't be removed.
 */
object `package`

final class All[L <: TList](val underlying: Prod[L]) extends AnyVal

object All {
  def apply[L <: TList](implicit ev: All[L]): All[L] = ev

  implicit def materializeAll[L <: TList]: All[L] =
    macro internal.EvidenceMacros.materializeAll[L]
}

final class FirstK[L <: TListK, A](val underlying: CopK[L, A]) extends AnyVal

object FirstK {
  def apply[L <: TListK, A](implicit ev: FirstK[L, A]): FirstK[L, A] = ev

  implicit def materializeFirstK[L <: TListK, A]: FirstK[L, A] =
    macro internal.EvidenceMacros.materializeFirstK[L, A]

}

final class FirstH[L <: TListH, F[_]](val underlying: CopH[L, F]) extends AnyVal

object FirstH {
  def apply[L <: TListH, F[_]](implicit ev: FirstH[L, F]): FirstH[L, F] = ev

  implicit def materializeFirstH[L <: TListH, F[_]]: FirstH[L, F] =
    macro internal.EvidenceMacros.materializeFirstH[L, F]
}
