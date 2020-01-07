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

package scalaz
package iotaz
package syntax

import scala.AnyVal
import iotaz.evidence._

trait EvidenceSyntax {
  def firstK[L <: TListK, A](implicit ev: FirstK[L, A]): CopK[L, A] =
    ev.underlying
}

final class InjectOps[A](val a: A) extends AnyVal {
  def inject[B <: Cop[_]](implicit ev: Cop.Inject[A, B]): B =
    ev.inj(a)
}

trait InjectSyntax {
  implicit def toInjectOps[A](a: A): InjectOps[A] = new InjectOps(a)
}

final class InjectKOps[F[_], A](val fa: F[A]) extends AnyVal {
  def injectK[G[α] <: CopK[_, α]](implicit ev: CopK.Inject[F, G]): G[A] =
    ev.inj(fa)
}

trait InjectKSyntax {
  implicit def toInjectKOps[F[_], A](fa: F[A]): InjectKOps[F, A] =
    new InjectKOps(fa)
}
