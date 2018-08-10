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
// Optimised String lookup
private[jsonformat] sealed abstract class StringyMap[A] {
  final def apply(s: String) = {
    val got = unsafe(s)
    if (got == null) sys.error("impossible") // careful now...
    got
  }
  protected def unsafe(s: String): A
  def get(s: String): Maybe[A] = Maybe.fromNullable(unsafe(s))
}
private[jsonformat] final class StringyHashMap[A](
  private[this] val hashmap: java.util.HashMap[String, A]
) extends StringyMap[A] {
  override def unsafe(s: String) = hashmap.get(s)
}
private[jsonformat] final class StringyIList[A >: Null](
  private[this] val entries: IList[(String, A)]
) extends StringyMap[A] {
  @tailrec private[this] final def find(
    rem: IList[(String, A)],
    s: String
  ): A =
    rem match {
      case _: INil[_] => null
      case c: ICons[_] =>
        val k = c.head._1
        if ((k.eq(s)) || (k.hashCode == s.hashCode && k == s)) c.head._2
        else find(c.tail, s)
    }

  override def unsafe(s: String) = find(entries, s)
}
private[jsonformat] final class StringySeq[A >: Null](
  private[this] val entries: Seq[(String, A)]
) extends StringyMap[A] {
  override def unsafe(s: String): A = {
    entries.foreach { e =>
      val k = e._1
      if ((k.eq(s)) || (k.hashCode == s.hashCode && k == s)) return e._2
    }
    null
  }
}
private[jsonformat] object StringyMap {
  private[this] val magic: Int = 5 // according to perf tests in scala-library

  def apply[A >: Null](entries: IList[(String, A)]): StringyMap[A] = {
    @tailrec def short(rem: IList[(String, A)], acc: Int): Boolean = rem match {
      case _: INil[_] => true
      case c: ICons[_] =>
        if (acc >= magic) false
        else short(c.tail, acc + 1)
    }

    if (short(entries, 0)) {
      new StringyIList(entries)
    } else {
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
  def apply[A >: Null](entries: Seq[(String, A)]): StringyMap[A] =
    if (entries.length < magic) {
      new StringySeq(entries)
    } else {
      val hashmap = new java.util.HashMap[String, A]
      entries.foreach(e => hashmap.put(e._1, e._2))
      new StringyHashMap(hashmap)
    }
}
// scalafix:on
