// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz._

import org.scalatest.Matchers._
import org.scalactic.source.Position

object XTestUtils {
  import XDecoder.ops._

  implicit class BetterEitherValuesOps[L, R](e: Either[L, R]) {
    def rightValue(implicit P: Position): R = e match {
      case Left(left)   => fail(s"got Left: $left")
      case Right(right) => right
    }

    def leftValue(implicit P: Position): L = e match {
      case Left(left)   => left
      case Right(right) => fail(s"got Right: $right")
    }
  }

  implicit class DisjunctionValuesOps[L, R](e: L \/ R) {
    def rightValue(implicit P: Position): R = e match {
      case -\/(left)  => fail(s"got Left: $left")
      case \/-(right) => right
    }

    def leftValue(implicit P: Position): L = e match {
      case -\/(left)  => left
      case \/-(right) => fail(s"got Right: $right")
    }
  }

  implicit class XNodeHelper(xml: XNode) {
    def as[T: XDecoder](implicit P: Position): T = xml.decode[T].rightValue
  }

}
