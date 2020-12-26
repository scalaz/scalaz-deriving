// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package fommil

import magnolia._
import scalaz._, Scalaz._

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
