/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package scalaz.annotation

import scala.annotation.Annotation

/**
 * Generates boilerplate for implicit evidence on companion objects via
 * `scalaz.macros.DerivingMacro.deriving`
 */
final class deriving(val typeclasses: AnyRef*) extends Annotation
