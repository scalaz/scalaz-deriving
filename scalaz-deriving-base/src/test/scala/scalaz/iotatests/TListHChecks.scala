/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

package scalaz
package iotatests

import scala._
import iotaz._

object TListHChecks {

  import TListH.Compute
  import TListH.Op._
  import TListH.::

  def check[L <: TListH, O <: TListH](implicit ev: Compute.Aux[L, O]): Unit = ()

  check[Reverse[Functor :: Monad :: TNilH], Monad :: Functor :: TNilH]

}
