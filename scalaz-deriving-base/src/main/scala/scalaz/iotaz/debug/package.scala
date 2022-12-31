// Copyright: 2017 - 2023 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

// Derived from https://github.com/frees-io/iota
//
// Copyright (C) 2017-2018 Andy Scott.
// Copyright (c) 2017-2018 47 Degrees. <http://47deg.com>
// All rights reserved.
//
// https://github.com/frees-io/iota/blob/v0.3.10/LICENSE
// https://github.com/frees-io/iota/blob/v0.3.10/NOTICE

package scalaz.iotaz

/**
 * Implicit options to configure/control Iota's macros
 */
package object debug {
  object optionTypes {
    sealed trait ShowTrees
    sealed trait ShowCache
    sealed trait ShowAborts
  }
  import optionTypes._

  object options {

    /**
     * Import this value to have Iota print the macro generated code
     * to the console during compilation
     */
    implicit val ShowTrees: ShowTrees = new ShowTrees {}

    /**
     * Import this value to have Iota print the cached computations
     * during macro expansion
     */
    implicit val ShowCache: ShowCache = new ShowCache {}

    /**
     * Import this value to have Iota print aborted instance
     * materialization for [[TList]] and [[KList]] helpers
     */
    implicit val ShowAborts: ShowAborts = new ShowAborts {}
  }
}
