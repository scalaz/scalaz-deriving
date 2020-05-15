// Copyright: 2017 - 2020 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package scalaz
package iotatests

import java.lang.String
import scala._
import iotaz._

import org.scalacheck._

object ProdTests extends Properties("ProdTests") {
  import TList.::
  import TList.Op._

  type StringIntDoubleL = String :: Int :: Double :: TNil

  val foo0: Prod[TCons[String, TCons[Int, TCons[Double, TNil]]]]          =
    Prod[StringIntDoubleL]("hello", 20, 6.62607004)
  val foo1: Prod[Reverse[TCons[String, TCons[Int, TCons[Double, TNil]]]]] =
    Prod[Reverse[StringIntDoubleL]](6.62607004, 20, "hello")

}
