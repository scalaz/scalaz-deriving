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

/** A heterogenous list of type constructors */
trait TListK

object TListK {

  /** A syntactic sugar alias for [[TConsK]] */
  type ::[H[_], T <: TListK] = TConsK[H, T]

  /** A syntactic sugar alias for [[TConsK]] */
  type :::[H[_], T <: TListK] = TConsK[H, T]

  /**
   * A type class that witnesses the position of type constructor `F` in type
   * constructor list `L`
   */
  trait Pos[L <: TListK, F[_]] {
    def index: Int
  }

  object Pos {
    def apply[L <: TListK, F[_]](implicit ev: Pos[L, F]): Pos[L, F] = ev
    implicit def materializePos[L <: TListK, F[_]]: Pos[L, F] =
      macro internal.TypeListMacros.materializeTListKPos[L, F]
  }

  object Op {
    type Concat[L <: TListK, R <: TListK] <: TListK
    type Reverse[L <: TListK] <: TListK
    type Take[N <: SingletonInt, L <: TListK] <: TListK
    type Drop[N <: SingletonInt, L <: TListK] <: TListK
    type Remove[K[_], L <: TListK] <: TListK
  }

  trait Compute[L <: TListK] {
    type Out <: TListK
  }

  object Compute {
    type Aux[L <: TListK, O <: TListK] = Compute[L] { type Out = O }

    def apply[L <: TListK](implicit ev: Compute[L]): Compute.Aux[L, ev.Out] = ev
    implicit def materializeCompute[L <: TListK, O <: TListK]: Aux[L, O] =
      macro internal.TypeListMacros.materializeTListKCompute[L, O]
  }

}
