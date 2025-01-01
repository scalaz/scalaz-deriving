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

import scala._, Predef._
import scala.collection.immutable.Seq

/** A product of types captured by type list `LL` */
final class Prod[LL <: TList] private (
  val values: Seq[Any]
) {
  type L = LL

  override def equals(anyOther: Any): Boolean =
    anyOther match {
      case other: Prod[LL] => values == other.values
      case _               => false
    }

  override def hashCode(): Int =
    values.hashCode()

  override def toString: String =
    s"""Prod(${values.mkString(", ")})"""
}

object Prod {

  import scalaz.Isomorphism._
  def gen[A, R <: TList]: A <=> Prod[R] =
    macro internal.ProductMacros.prodGen[A, R]

  def apply[L <: TList](args: Any*): Prod[L] =
    macro internal.ProductMacros.prodApply[L]

  def unsafeApply[L <: TList](values: Seq[Any]): Prod[L] =
    new Prod[L](values)

}
