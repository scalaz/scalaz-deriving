/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package fommil

import magnolia.*
import scalaz.*
import scalaz.Scalaz.*

object MagnoliaShow {
  type Typeclass[A] = Show[A]

  def combine[A](ctx: CaseClass[Show, A]): Show[A] =
    Show.shows { a =>
      val bits = ctx.parameters.map { p =>
        p.label + "=" + p.typeclass.shows(p.dereference(a))
      }.toList
      ctx.typeName.short + "(" + bits.intercalate(",") + ")"
    }

  def dispatch[A](ctx: SealedTrait[Show, A]): Show[A] =
    Show.show { a =>
      ctx.dispatch(a) { sub =>
        sub.typeclass.show(sub.cast(a))
      }
    }

  def gen[A]: Show[A] = macro Magnolia.gen[A]

}
