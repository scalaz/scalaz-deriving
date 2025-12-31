/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

package scalaz
package iotatests

import scala.*
import scalaz.iotaz.*

object TListKChecks {

  import TListK.::
  import TListK.Compute
  import TListK.Op.*

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
    OptionList[?]#L,
    ListOption[?]#L
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
