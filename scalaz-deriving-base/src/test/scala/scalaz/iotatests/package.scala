/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

package scalaz

package object iotatests {
  // needed by Scalacheck when no predef is used
  private[iotatests] implicit def identityView[A](a: A): A = a

  private[iotatests] val CopKNT = iotaz.CopK.NaturalTransformation
}
