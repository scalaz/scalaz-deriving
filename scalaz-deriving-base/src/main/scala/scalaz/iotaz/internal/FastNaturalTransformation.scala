// Copyright: 2017 - 2025 Sam Halliday
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
package internal

import scalaz.NaturalTransformation

/**
 * This exists soley as a marker for `NaturalTransformations`s generated by iota
 * macros
 */
trait FastNaturalTransformation[F[_], G[_]] extends NaturalTransformation[F, G]
