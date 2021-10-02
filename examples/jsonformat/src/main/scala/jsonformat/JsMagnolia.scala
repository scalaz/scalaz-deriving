// Copyright: 2017 - 2021 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import scala.annotation.Annotation

import magnolia._
import scalaz._, Scalaz._
import internal.StringyMap
import internal.FastToIList._

/**
 * Annotation for customising automatically derived instances of JsEncoder and
 * JsDecoder.
 *
 * @param nulls If used on a product field, will write out `null` values and
 *              decoding will fail if there are any missing fields (that do not
 *              have default values).
 *
 *              The default behaviour is to skip fields that encode to `JsNull`,
 *              to use default values for missing values, and if there is no
 *              default value to decode a `JsNull`.
 *
 * @param field If used on a coproduct, will determine the typehint field for
 *              disambiguating products. Defaults to "type".
 *
 *              If used on a product that is part of a coproduct, if the product
 *              encodes to a non-JsObject, will be used for the field containing
 *              the value. Defaults to "xvalue".
 *
 *              If used on a product field, determines the name of the JSON
 *              field. Defaults to the same name as the scala field.
 *
 * @param hint  If used on a product that is part of a coproduct will be used for
 *              the typehint value. Defaults to the short type name.
 */
case class json(
  nulls: Boolean,
  field: Option[String],
  hint: Option[String]
) extends Annotation
object json {
  object nulls {
    def unapply(j: json): Boolean = j.nulls
  }
  object field {
    def unapply(j: json): Option[String] = j.field
  }
  object hint  {
    def unapply(j: json): Option[String] = j.hint
  }
}

object JsMagnoliaEncoder {
  type Typeclass[A] = JsEncoder[A]

  def combine[A](ctx: CaseClass[JsEncoder, A]): JsEncoder[A] =
    new JsEncoder[A] {
      private[this] val nulls      = ctx.parameters.map { p =>
        p.annotations.collectFirst { case json.nulls() =>
          true
        }.getOrElse(false)
      }.toArray
      private[this] val fieldnames = ctx.parameters.map { p =>
        p.annotations.collectFirst { case json.field(name) =>
          name
        }.getOrElse(p.label)
      }.toArray

      def toJson(a: A): JsValue = {
        val empty  = IList.empty[(String, JsValue)]
        val fields = ctx.parameters.foldRight(empty) { (p, acc) =>
          p.typeclass.toJson(p.dereference(a)) match {
            case JsNull if !nulls(p.index) => acc
            case value                     => (fieldnames(p.index) -> value) :: acc
          }
        }
        JsObject(fields)
      }
    }

  def dispatch[A](ctx: SealedTrait[JsEncoder, A]): JsEncoder[A] =
    new JsEncoder[A] {
      private[this] val field   = ctx.annotations.collectFirst {
        case json.field(name) => name
      }.getOrElse("type")
      private[this] val hints   = ctx.subtypes.map { s =>
        s.annotations.collectFirst { case json.hint(name) =>
          field -> JsString(name)
        }.getOrElse(field -> JsString(s.typeName.short))
      }.toArray
      private[this] val xvalues = ctx.subtypes.map { s =>
        s.annotations.collectFirst { case json.field(name) =>
          name
        }.getOrElse("xvalue")
      }.toArray

      def toJson(a: A): JsValue =
        ctx.dispatch(a) { sub =>
          val hint = hints(sub.index)
          sub.typeclass.toJson(sub.cast(a)) match {
            case JsObject(fields) => JsObject(hint :: fields)
            case other            =>
              JsObject(hint :: (xvalues(sub.index) -> other) :: IList.empty)
          }
        }
    }

  def gen[A]: JsEncoder[A] = macro Magnolia.gen[A]
}

object JsMagnoliaDecoder {
  type Typeclass[A] = JsDecoder[A]

  import JsDecoder.fail

  def combine[A](ctx: CaseClass[JsDecoder, A]): JsDecoder[A] =
    new JsDecoder[A] {
      private[this] val nulls = ctx.parameters.map { p =>
        p.annotations.collectFirst { case json.nulls() =>
          true
        }.getOrElse(false)
      }.toArray

      private[this] val fieldnames = ctx.parameters.map { p =>
        p.annotations.collectFirst { case json.field(name) =>
          name
        }.getOrElse(p.label)
      }.toArray

      private[this] val eitherStringMonadic: mercator.Monadic[String \/ *] =
        new mercator.Monadic[String \/ *] {
          override def point[X](value: X): String \/ X                       =
            \/-(value)
          override def flatMap[X, Y](
            from: String \/ X
          )(fn: X => String \/ Y): String \/ Y =
            from.flatMap(fn)
          override def map[X, Y](from: String \/ X)(fn: X => Y): String \/ Y =
            from.map(fn)
        }

      private[this] val EitherStringIsCovariant: IsCovariant[String \/ *] =
        IsCovariant[String \/ *]

      def fromJson(j: JsValue): String \/ A =
        j match {
          case obj @ JsObject(_) =>
            val lookup = StringyMap(obj.fields, fieldnames.length)
            ctx.constructMonadic[String \/ *, Param[JsDecoder, A]#PType] { p =>
              val field = fieldnames(p.index)
              lookup
                .get(field)
                .into {
                  case Maybe.Just(value) =>
                    EitherStringIsCovariant.widen(p.typeclass.fromJson(value))
                  case _                 =>
                    p.default match {
                      case Some(default)          =>
                        \/-[String, Param[JsDecoder, A]#PType](default)
                      case None if nulls(p.index) =>
                        s"missing field '$field'"
                          .left[Param[JsDecoder, A]#PType]
                      case None                   =>
                        EitherStringIsCovariant.widen(
                          p.typeclass.fromJson(JsNull)
                        )
                    }
                }
            }(eitherStringMonadic)
          case other             => fail("JsObject", other)
        }
    }

  def dispatch[A](ctx: SealedTrait[JsDecoder, A]): JsDecoder[A] =
    new JsDecoder[A] {
      private[this] val subtype  = StringyMap(
        ctx.subtypes.mapToIList { s =>
          s.annotations.collectFirst { case json.hint(name) =>
            name
          }.getOrElse(s.typeName.short) -> s
        },
        Int.MaxValue
      )
      private[this] val typehint = ctx.annotations.collectFirst {
        case json.field(name) => name
      }.getOrElse("type")
      private[this] val xvalues  = ctx.subtypes.map { sub =>
        sub.annotations.collectFirst { case json.field(name) =>
          name
        }.getOrElse("xvalue")
      }.toArray

      def fromJson(j: JsValue): String \/ A =
        j match {
          case obj @ JsObject(fields) =>
            val lookup = StringyMap(fields, 2)
            lookup.get(typehint) match {
              case Maybe.Just(JsString(h)) =>
                subtype.get(h) match {
                  case Maybe.Empty()   => fail(s"a valid '$h'", obj)
                  case Maybe.Just(sub) =>
                    val xvalue = xvalues(sub.index)
                    val value  = lookup.get(xvalue).getOrElse(obj)
                    IsCovariant[String \/ *].widen(
                      sub.typeclass.fromJson(value)
                    )
                }
              case _                       => fail(s"JsObject with '$typehint' field", obj)
            }
          case other                  => fail("JsObject", other)
        }
    }

  def gen[A]: JsDecoder[A] = macro Magnolia.gen[A]
}
