// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import scala.annotation.{ tailrec, Annotation }

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
          import mercator._
          val lookup = StringyMap(obj.fields)
          ctx.constructMonadic { p =>
            val field = fieldnames(p.index)
            lookup
              .get(field)
              .into {
                case Maybe.Just(value) => p.typeclass.fromJson(value)
                case _ =>
                  p.default match {
                    case Some(default) => \/-(default)
                    case None if nulls(p.index) =>
                      s"missing field '$field'".left
                    case None => p.typeclass.fromJson(JsNull)
                  }
              }
          }
        case other => fail("JsObject", other)
      }
    }

  def dispatch[A](ctx: SealedTrait[JsDecoder, A]): JsDecoder[A] =
    new JsDecoder[A] {
      private val subtype = ctx.subtypes.map { s =>
        s.annotations.collectFirst {
          case json.hint(name) => name
        }.getOrElse(s.typeName.short) -> s
      }.toMap
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
            case \/-(JsString(h)) =>
              subtype.get(h) match {
                case None => fail(s"a valid '$h'", obj)
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
// Optimised String lookup. Performance testing has shown that the Java HashMap
// is faster to create by about a factor of 5 than a Scala HashMap (and the gap
// grows for larger collections).
//
// Java HashMap is faster to lookup than an IList, for all sizes (even 1 and 2
// elements!).
//
// However, the scala HashMap is faster to lookup by about 20% than a Java
// HashMap. so the usecase for this StringyMap is when everything needs to be
// looked up approximately once, and then thrown away. For long lived stringy
// map, prefer a scala HashMap.
//
// In other words, use toMap to cache subtype lookup, and StringyMap for field
// lookup.
private[jsonformat] abstract class StringyMap[A] {
  def get(s: String): Maybe[A]
}
private[jsonformat] final class StringyHashMap[A](
  private[this] val hashmap: java.util.HashMap[String, A]
) extends StringyMap[A] {
  def get(s: String) = Maybe.fromNullable(hashmap.get(s))
}
private[jsonformat] final class StringyEmptyMap[A] extends StringyMap[A] {
  def get(s: String): Maybe[A] = Maybe.empty
}
private[jsonformat] final class StringySingleMap[A](k: String, a: A)
    extends StringyMap[A] {
  def get(s: String) = if (k == s) Maybe.just(a) else Maybe.empty
}
private[jsonformat] object StringyMap {
  def apply[A >: Null](entries: IList[(String, A)]): StringyMap[A] =
    entries match {
      case INil()              => new StringyEmptyMap()
      case ICons(head, INil()) => new StringySingleMap(head._1, head._2)
      case _ =>
        val hashmap = new java.util.HashMap[String, A]
        @tailrec def visit(rem: IList[(String, A)]): Unit = rem match {
          case _: INil[_] => ()
          case c: ICons[_] =>
            hashmap.put(c.head._1, c.head._2)
            visit(c.tail)
        }
        visit(entries)
        new StringyHashMap(hashmap)
    }
}
// scalafix:on
