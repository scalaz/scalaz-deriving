// Copyright: 2017 - 2023 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package scalaz

package object iotatests {
  // needed by Scalacheck when no predef is used
  private[iotatests] implicit def identityView[A](a: A): A = a

  private[iotatests] val CopKNT = iotaz.CopK.NaturalTransformation
}
