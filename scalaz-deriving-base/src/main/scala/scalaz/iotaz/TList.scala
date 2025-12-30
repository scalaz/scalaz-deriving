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

import scala._

/** A heterogenous list of types */
trait TList

object TList {

  /** A syntactic sugar alias for [[TCons]] */
  type ::[H, T <: TList] = TCons[H, T]

  /** A syntactic sugar alias for [[TCons]] */
  type :::[H, T <: TList] = TCons[H, T]

  /**
   * A type class that witnesses the position of type `A` in type
   * list `L`
   */
  trait Pos[L <: TList, A] {
    def index: Int
  }

  object Pos {
    def apply[L <: TList, A](implicit ev: Pos[L, A]): Pos[L, A] = ev
    implicit def materializePos[L <: TList, A]: Pos[L, A] =
      macro internal.TypeListMacros.materializeTListPos[L, A]
  }

  trait Length[L <: TList] {
    type Value <: SingletonInt
    def value: Value
  }

  object Length {
    type Aux[L <: TList, N <: SingletonInt] = Length[L] {
      type Value = N
    }

    def apply[L <: TList](implicit ev: Length[L]): Length.Aux[L, ev.Value] = ev
    implicit def materializeLength[L <: TList]: Length[L] =
      macro internal.TypeListMacros.materializeTListLength[L]
  }

  object Op {
    type Concat[L <: TList, R <: TList] <: TList
    type Reverse[L <: TList] <: TList
    type Take[N <: SingletonInt, L <: TList] <: TList
    type Drop[N <: SingletonInt, L <: TList] <: TList
    type Remove[T, L <: TList] <: TList

    type Map[F[_], L <: TList] <: TList
  }

  trait Compute[L <: TList] {
    type Out <: TList
  }

  object Compute {
    type Aux[L <: TList, O <: TList] = Compute[L] { type Out = O }

    def apply[L <: TList](implicit ev: Compute[L]): Compute.Aux[L, ev.Out] = ev
    implicit def materializeCompute[L <: TList, O <: TList]: Aux[L, O] =
      macro internal.TypeListMacros.materializeTListCompute[L, O]
  }

}
