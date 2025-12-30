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

package scalaz

import scala._
import scala.reflect.ClassTag

package object iotaz {

  /** The terminal element of a type list */
  type TNil <: TList

  /**
   * A type list characterized by a head type and a list of tail types
   *
   * @tparam H the head type
   * @tparam T the list of tail types
   */
  type TCons[H, T <: TList] <: TList

  /** The terminal element of a type constructor list */
  type TNilK <: TListK

  /**
   * A type constructor list characterized by a head type
   * constructor and a list of tail type constructors
   *
   * @tparam H the head type constructor
   * @tparam T the list of tail type constructors
   */
  type TConsK[H[_], T <: TListK] <: TListK

  /** The terminal element of a type list for shapes `T[_[_]]` */
  type TNilH <: TListH

  /**
   * A type list characterized by a head type and a list of tail types
   * all of shape `T[_[_]]`
   *
   * @tparam H the head type constructor
   * @tparam T the list of tail type constructors
   */
  type TConsH[H[_[_]], T <: TListH] <: TListH

  // -- internals --

  private[iotaz] type SingletonInt = Int with Singleton

  // either compat

  private[iotaz] implicit final class ToEitherCompatOps[A](
    private val obj: A
  ) extends AnyVal {
    def asLeft[B]: Either[A, B]  = Left(obj)
    def asRight[B]: Either[B, A] = Right(obj)
  }

  import scalaz.NonEmptyList
  import scalaz.NotNothing
  import scalaz.Validation
  import scalaz.ValidationNel

  private[iotaz] implicit final class NonEmptyListCompanionOps(
    private val companion: NonEmptyList.type
  ) extends AnyVal {
    def one[A](head: A): scalaz.NonEmptyList[A] = scalaz.NonEmptyList(head)
  }

  type Avowal[A, B]    = Validation[A, B]
  type AvowalNel[A, B] = ValidationNel[A, B]

  object Avowal {
    def yes[A, B](b: B): Avowal[A, B]      = Validation.success(b)
    def no[A, B](a: A): Avowal[A, B]       = Validation.failure(a)
    def noNel[A, B](a: A): AvowalNel[A, B] = Validation.failureNel(a)

    def catching[T >: Null <: Throwable]: CatchingPartiallyApplied[T] =
      new CatchingPartiallyApplied[T]

    private[Avowal] final class CatchingPartiallyApplied[
      T >: Null <: Throwable
    ] {
      def apply[A](
        f: =>A
      )(implicit T: ClassTag[T], NT: NotNothing[T]): Avowal[T, A] =
        Validation.fromTryCatchThrowable[A, T](f)
    }
  }

  private[iotaz] implicit final class EitherToAvowalOps[A, B](
    private val eab: Either[A, B]
  ) extends AnyVal {
    def toAvowal: Avowal[A, B]                 =
      eab match {
        case Left(a)  => Avowal.no(a)
        case Right(b) => Avowal.yes(b)
      }
    def toAvowalNel[AA >: A]: AvowalNel[AA, B] =
      eab match {
        case Left(a)  => Avowal.noNel(a)
        case Right(b) => Avowal.yes(b)
      }
  }
}
