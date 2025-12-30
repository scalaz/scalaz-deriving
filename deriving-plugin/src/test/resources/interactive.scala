/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

import scalaz.annotation.deriving

trait Format[A]
object Format {
  implicit val string: Format[String] = null
}

package wibble {
  @deriving(Format)
  sealed trait Ba@baz@z

  @deriving(Format)
  final case class F@foo@oo(string: String, int: Int) extends Baz

  @deriving(Format)
  final case class G@gaz@az[T](t: T) extends Baz
}

object Wobble {
  import wibble._

  implicitly[Format[Baz]]
  implicitly[Format[Foo]]
  implicitly[Format[Gaz[String]]]

}
