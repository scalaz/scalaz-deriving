/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

package scalaz
package iotaz
package internal

import catryoshka.*
import java.lang.String
import org.scalacheck.*
import org.scalacheck.Prop.*
import scala.*
import scala.Predef.ArrowAssoc
import scalaz.std.either.*

object TypeListParserChecks extends Properties("TypeListParsers") {

  val checks: TypeListParserChecks = new TypeListParserChecks(
    IotaReflectiveToolbelt()
  )

  checks.tlists.foreach { case (in, out) =>
    property(s"parse TList $in") = Corecursive[checks.Node]
      .anaM(in)(checks.tb.tlistParser) ?= Right(out)
  }

  checks.tlistks.foreach { case (in, out) =>
    property(s"parse TListK $in") = Corecursive[checks.Node]
      .anaM(in)(checks.tb.tlistkParser) ?= Right(out)
  }

}

class TypeListParserChecks(
  override val tb: Toolbelt with TypeListAST with TypeListParsers
) extends TestTreeHelper(tb) {

  import TList.::
  import TList.Op.Reverse as TReverse
  import tb.u.Type

  val tlists: List[(Type, Node)] = List(
    t[TNil] -> nnil,
    t[Int :: TNil] -> cons[Int](),
    t[String :: Int :: TNil] -> cons[String](cons[Int]()),
    t[TReverse[TNil]] -> reverse(nnil)
  )

  import TListK.Op.Reverse as KReverse

  val tlistks: List[(Type, Node)] = List(
    t[KReverse[TNilK]] -> reverse(nnil)
  )

}
