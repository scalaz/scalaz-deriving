// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import magnolia._
import scalaz._

object MagnoliaEqual {
  type Typeclass[T] = Equal[T]

  def combine[T](ctx: CaseClass[Equal, T]): Equal[T] =
    new Equal[T] {
      def equal(a1: T, a2: T) =
        ctx.parameters.forall { p =>
          val b1 = p.dereference(a1)
          val b2 = p.dereference(a2)
          quick(b1, b2) || p.typeclass.equal(b1, b2)
        }
    }

  def dispatch[T](ctx: SealedTrait[Equal, T]): Equal[T] =
    new Equal[T] {
      def equal(a1: T, a2: T): Boolean =
        quick(a1, a2) || ctx.dispatch(a1) { sub =>
          sub.cast
            .isDefinedAt(a2) && sub.typeclass.equal(sub.cast(a1), sub.cast(a2))
        }
    }

  def gen[T]: Equal[T] = macro Magnolia.gen[T]

  @inline private final def quick(a: Any, b: Any): Boolean =
    a.asInstanceOf[AnyRef].eq(b.asInstanceOf[AnyRef])

}
