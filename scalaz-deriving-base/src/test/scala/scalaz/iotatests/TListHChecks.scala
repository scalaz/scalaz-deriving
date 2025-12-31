/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

package scalaz
package iotatests

import scala.*
import scalaz.iotaz.*

object TListHChecks {

  import TListH.::
  import TListH.Compute
  import TListH.Op.*

  def check[L <: TListH, O <: TListH](implicit ev: Compute.Aux[L, O]): Unit = ()

  check[Reverse[Functor :: Monad :: TNilH], Monad :: Functor :: TNilH]

}
