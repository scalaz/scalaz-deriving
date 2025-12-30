/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package scalaz.annotation

import scala.annotation.Annotation

/**
 * Generates boilerplate for implicit evidence on companion objects for single
 * valued data types via `scalaz.macros.DerivingMacro.xderiving`
 */
final class xderiving(val typeclasses: AnyRef*) extends Annotation
