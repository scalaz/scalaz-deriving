// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import scala.annotation.Annotation

import magnolia._
import scalaz._, Scalaz._

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
  object hint {
    def unapply(j: json): Option[String] = j.hint
  }
}

object JsMagnoliaEncoder {
  type Typeclass[A] = JsEncoder[A]

  def combine[A](ctx: CaseClass[JsEncoder, A]): JsEncoder[A] =
    new JsEncoder[A] {
      private val nulls = ctx.parameters.map { p =>
        p.annotations.collectFirst {
          case json.nulls() => true
        }.getOrElse(false)
      }.toArray
      private val fieldnames = ctx.parameters.map { p =>
        p.annotations.collectFirst {
          case json.field(name) => name
        }.getOrElse(p.label)
      }.toArray

      def toJson(a: A): JsValue = {
        val empty = IList.empty[(String, JsValue)]
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
      private val field = ctx.annotations.collectFirst {
        case json.field(name) => name
      }.getOrElse("type")
      private val hints = ctx.subtypes.map { s =>
        s.annotations.collectFirst {
          case json.hint(name) => field -> JsString(name)
        }.getOrElse(field -> JsString(s.typeName.short))
      }.toArray
      private val xvalues = ctx.subtypes.map { s =>
        s.annotations.collectFirst {
          case json.field(name) => name
        }.getOrElse("xvalue")
      }.toArray

      def toJson(a: A): JsValue = ctx.dispatch(a) { sub =>
        val hint = hints(sub.index)
        sub.typeclass.toJson(sub.cast(a)) match {
          case JsObject(fields) => JsObject(hint :: fields)
          case other =>
            JsObject(hint :: (xvalues(sub.index) -> other) :: IList.empty)
        }
      }
    }

  def gen[A]: JsEncoder[A] = macro Magnolia.gen[A]
}

object JsMagnoliaDecoder {
  type Typeclass[A] = JsDecoder[A]

  import JsDecoder.ops._
  import JsDecoder.fail

  def combine[A](ctx: CaseClass[JsDecoder, A]): JsDecoder[A] =
    new JsDecoder[A] {
      private val nulls = ctx.parameters.map { p =>
        p.annotations.collectFirst {
          case json.nulls() => true
        }.getOrElse(false)
      }.toArray

      private val fieldnames = ctx.parameters.map { p =>
        p.annotations.collectFirst {
          case json.field(name) => name
        }.getOrElse(p.label)
      }.toArray

      def fromJson(j: JsValue): String \/ A = j match {
        case obj @ JsObject(_) =>
          ctx.constructEither { p =>
            val field = fieldnames(p.index)
            obj
              .get(field)
              .into {
                case \/-(value) => p.typeclass.fromJson(value)
                case err @ -\/(_) =>
                  p.default match {
                    case Some(default)          => \/-(default)
                    case None if nulls(p.index) => err
                    case None                   => p.typeclass.fromJson(JsNull)
                  }
              }
              .toEither
          }.disjunction
        case other => fail("JsObject", other)
      }
    }

  def dispatch[A](ctx: SealedTrait[JsDecoder, A]): JsDecoder[A] =
    new JsDecoder[A] {
      private val hints = StringyMap(
        ctx.subtypes.map { s =>
          s.typeName.full -> s.annotations.collectFirst {
            case json.hint(name) => name
          }.getOrElse(s.typeName.short)
        }
      )
      private val typehint = ctx.annotations.collectFirst {
        case json.field(name) => name
      }.getOrElse("type")
      private val xvalues = ctx.subtypes.map { sub =>
        sub.annotations.collectFirst {
          case json.field(name) => name
        }.getOrElse("xvalue")
      }.toArray

      def fromJson(j: JsValue): String \/ A = j match {
        case obj @ JsObject(_) =>
          obj.get(typehint) match {
            case \/-(JsString(hint)) =>
              ctx.subtypes.find(s => hints(s.typeName.full) == hint) match {
                case None => fail(s"a valid '$hint'", obj)
                case Some(sub) =>
                  val xvalue = xvalues(sub.index)
                  val value  = obj.get(xvalue).getOrElse(obj)
                  sub.typeclass.fromJson(value)
              }
            case _ => fail(s"JsObject with '$typehint' field", obj)
          }
        case other => fail("JsObject", other)
      }
    }

  def gen[A]: JsDecoder[A] = macro Magnolia.gen[A]
}

// scalafix:off
//
// Optimised String lookup, works even faster if keys are interned.
private[jsonformat] final class StringyMap[A](
  private[this] val entries: Array[(String, A)]
) {
  private[this] val hashmap = {
    val m = new java.util.HashMap[String, A]
    entries.foreach {
      case (s, a) => m.put(s, a)
    }
    m
  }
  private[this] val keys = entries.map(_._1).toArray

  // only if you're certain this is total...
  def apply(s: String): A = {
    var i = 0
    while (i < keys.length) {
      if (keys(i).eq(s)) return entries(i)._2
      i += 1
    }
    hashmap.get(s)
  }

}
private[jsonformat] object StringyMap {
  def apply[F[_]: Foldable, A](a: F[(String, A)]): StringyMap[A] =
    apply[A](a.toList)
  def apply[A](a: Seq[(String, A)]): StringyMap[A] =
    new StringyMap(a.toArray)
}
// scalafix:on
