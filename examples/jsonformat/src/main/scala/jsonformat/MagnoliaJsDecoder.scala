// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import scala.util.control.NoStackTrace

import magnolia._
import scalaz._

object MagnoliaJsDecoder {
  type Typeclass[A] = JsDecoder[A]

  import JsDecoder.ops._

  // traverse is not supported so we must try/catch
  private case class Fail(msg: String) extends Exception with NoStackTrace

  def combine[A](ctx: CaseClass[JsDecoder, A]): JsDecoder[A] = {
    case obj @ JsObject(_) =>
      try {
        val success = ctx.construct { p =>
          val value = obj.get(p.label).getOrElse(JsNull)
          p.typeclass.fromJson(value) match {
            case \/-(got)  => got
            case -\/(fail) => throw new Fail(fail) // scalafix:ok
          }
        }
        \/-(success)
      } catch {
        case Fail(msg) => -\/(msg)
      }
    case other => JsDecoder.fail("JsObject", other)
  }

  def dispatch[A](ctx: SealedTrait[JsDecoder, A]): JsDecoder[A] = {
    case obj @ JsObject(_) =>
      obj.get("type") match {
        case \/-(JsString(hint)) =>
          ctx.subtypes.find(_.typeName.short == hint) match {
            case None => JsDecoder.fail(s"a valid $hint", obj)
            case Some(sub) =>
              val value = obj.get("xvalue").getOrElse(obj)
              sub.typeclass.fromJson(value)
          }

        case _ =>
          JsDecoder.fail("JsObject with type", obj)
      }
    case other => JsDecoder.fail("JsObject", other)
  }

  def gen[A]: JsDecoder[A] = macro Magnolia.gen[A]
}
