// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import magnolia._
import scalaz._, Scalaz._

object MagnoliaJsEncoder {
  type Typeclass[A] = JsEncoder[A]

  def combine[A](ctx: CaseClass[JsEncoder, A]): JsEncoder[A] = { a =>
    val fields = ctx.parameters.flatMap { p =>
      p.typeclass.toJson(p.dereference(a)) match {
        case JsNull => Nil
        case value  => (p.label -> value) :: Nil
      }
    }
    JsObject(fields.toList.toIList)
  }

  def dispatch[A](ctx: SealedTrait[JsEncoder, A]): JsEncoder[A] =
    a =>
      ctx.dispatch(a) { sub =>
        val hint = "type" -> JsString(sub.typeName.short)
        sub.typeclass.toJson(sub.cast(a)) match {
          case JsObject(fields) => JsObject(hint :: fields)
          case other            => JsObject(IList(hint, "xvalue" -> other))
        }
      }

  def gen[A]: JsEncoder[A] = macro Magnolia.gen[A]
}
