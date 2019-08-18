// Copyright: 2017 - 2019 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package scalaz
package iotaz
package internal

import java.lang.String
import scala._
import scala.Predef.ArrowAssoc

import org.scalacheck._
import org.scalacheck.Prop._

import scalaz.std.either._
import catryoshka._

object TypeListParserChecks extends Properties("TypeListParsers") {

  val checks = new TypeListParserChecks(IotaReflectiveToolbelt())

  checks.tlists.foreach {
    case (in, out) =>
      property(s"parse TList $in") = Corecursive[checks.Node]
        .anaM(in)(checks.tb.tlistParser) ?= Right(out)
  }

  checks.tlistks.foreach {
    case (in, out) =>
      property(s"parse TListK $in") = Corecursive[checks.Node]
        .anaM(in)(checks.tb.tlistkParser) ?= Right(out)
  }

}

class TypeListParserChecks(
  override val tb: Toolbelt with TypeListAST with TypeListParsers
) extends TestTreeHelper(tb) {

  import tb.u.Type

  import TList.::
  import TList.Op.{ Reverse => TReverse }

  val tlists: List[(Type, Node)] = List(
    t[TNil]                  -> nnil,
    t[Int :: TNil]           -> cons[Int](),
    t[String :: Int :: TNil] -> cons[String](cons[Int]()),
    t[TReverse[TNil]]        -> reverse(nnil)
  )

  import TListK.Op.{ Reverse => KReverse }

  val tlistks: List[(Type, Node)] = List(
    t[KReverse[TNilK]] -> reverse(nnil)
  )

}
