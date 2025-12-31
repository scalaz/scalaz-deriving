/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package jsonformat.internal

import scalaz.*

private[jsonformat] object FastToIList {

  implicit final class SeqExtras[A](private val self: Seq[A]) extends AnyVal {
    // there is no Foldable[Seq] so this is a suitable replacement
    def toIList: IList[A] = self.foldRight(IList.empty[A])(_ :: _)
    def mapToIList[B](f: A => B): IList[B] =
      self.foldRight(IList.empty[B])(f(_) :: _)
  }

  implicit final class MapExtras[A, B](private val self: Map[A, B])
      extends AnyVal {
    // name clash with the Foldable syntax
    def asIList: IList[(A, B)] = self.foldRight(IList.empty[(A, B)])(_ :: _)
    // def mapToIList[B](f: A => B): IList[B] = self.foldRight(IList.empty[B])(f(_) :: _)
  }

}
