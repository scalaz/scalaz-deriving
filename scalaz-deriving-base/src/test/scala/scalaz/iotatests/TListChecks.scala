// Copyright: 2017 - 2021 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package scalaz
package iotatests

import java.lang.String
import scala._
import iotaz._

object TListChecks {

  import TList.Compute
  import TList.Op._
  import TList.::

  def check[L <: TList, O <: TList](implicit ev: Compute.Aux[L, O]): Unit = ()

  check[Reverse[String :: Double :: TNil], Double :: String :: TNil]

  type StringIntL = String :: Int :: TNil
  type IntStringL = Int :: String :: TNil

  check[Reverse[StringIntL], IntStringL]
  check[Reverse[IntStringL], StringIntL]

  check[Concat[StringIntL, IntStringL], String :: Int :: Int :: String :: TNil]

  type StringInt = Cop[StringIntL]
  type IntString = Cop[IntStringL]

  check[Concat[
    StringInt#L,
    IntString#L
  ], String :: Int :: Int :: String :: TNil]

  check[Remove[String, StringIntL], Int :: TNil]
  check[Remove[Int, StringIntL], String :: TNil]
  check[Remove[Int, Int :: Int :: TNil], Int :: TNil]

  check[Map[Option, Int :: TNil], Option[Int] :: TNil]
  check[Map[List, String :: Double :: TNil], List[String] :: List[
    Double
  ] :: TNil]

  check[Map[
    λ[a => Int],
    String :: Double :: Long :: TNil
  ], Int :: Int :: Int :: TNil]

  check[Map[λ[a => Either[a, a]], String :: Double :: Long :: TNil], Either[
    String,
    String
  ] :: Either[Double, Double] :: Either[Long, Long] :: TNil]
}
