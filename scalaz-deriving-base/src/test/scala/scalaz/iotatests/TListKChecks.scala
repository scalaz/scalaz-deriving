// Copyright: 2017 - 2025 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package scalaz
package iotatests

import scala._
import iotaz._

object TListKChecks {

  import TListK.Compute
  import TListK.Op._
  import TListK.::

  def check[L <: TListK, O <: TListK](implicit ev: Compute.Aux[L, O]): Unit = ()

  check[Reverse[Option :: List :: TNilK], List :: Option :: TNilK]

  type OptionListL = Option :: List :: TNilK
  type ListOptionL = List :: Option :: TNilK

  check[Reverse[OptionListL], ListOptionL]
  check[Reverse[ListOptionL], OptionListL]

  check[Concat[
    OptionListL,
    ListOptionL
  ], Option :: List :: List :: Option :: TNilK]

  type OptionList[A] = CopK[OptionListL, A]
  type ListOption[A] = CopK[ListOptionL, A]

  check[Concat[
    OptionList[_]#L,
    ListOption[_]#L
  ], Option :: List :: List :: Option :: TNilK]

  check[Concat[Option :: TNilK, List :: TNilK], OptionListL]

  check[Concat[
    Reverse[OptionListL],
    OptionListL
  ], List :: Option :: Option :: List :: TNilK]

  check[Remove[Option, OptionListL], List :: TNilK]
  check[Remove[List, OptionListL], Option :: TNilK]
  check[Remove[List, List :: List :: TNilK], List :: TNilK]

}
