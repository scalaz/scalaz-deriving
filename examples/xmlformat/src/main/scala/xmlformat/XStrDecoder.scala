// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import java.util.UUID
import java.time.Instant

import scala.util.control.NonFatal

import shapeless.Typeable
import scalaz._, Scalaz._

@simulacrum.typeclass(generateAllOps = false)
trait XStrDecoder[A] { self =>
  import XStrDecoder.Out

  def fromXml(x: XString): Out[A]

  def map[B](f: A => B): XStrDecoder[B]                     = x => self.fromXml(x).map(f)
  def xmap[B](f: A => B, @unused g: B => A): XStrDecoder[B] = map(f)
  def emap[B](f: A => Out[B]): XStrDecoder[B]               = x => self.fromXml(x).flatMap(f)
}
object XStrDecoder extends XStrDecoderRefined with XStrDecoderStdlib {

  type Out[A] = String \/ A
  object ops extends ToXStrDecoderOps {
    implicit class XStrDecoderOps(private val x: XString) extends AnyVal {
      def decode[A: XStrDecoder]: Out[A] = XStrDecoder[A].fromXml(x)
    }
  }

  // WORKAROUND https://github.com/scalaz/scalaz/issues/1590
  private def str[A](f: String => A)(implicit A: Typeable[A]): XStrDecoder[A] =
    string.emap { s =>
      try \/-(f(s))
      catch {
        case NonFatal(_) => -\/(s"expected ${A.describe} in '$s'")
      }
    }

  implicit val string: XStrDecoder[String] = {
    case XString(text) => \/-(text.trim)
  }

  implicit val boolean: XStrDecoder[Boolean] = str(_.toBoolean)
  implicit val short: XStrDecoder[Short]     = str(_.toShort)
  implicit val int: XStrDecoder[Int]         = str(_.toInt)
  implicit val long: XStrDecoder[Long]       = str(_.toLong)
  implicit val float: XStrDecoder[Float]     = str(_.toFloat)
  implicit val double: XStrDecoder[Double]   = str(_.toDouble)
  implicit val uuid: XStrDecoder[UUID]       = str(UUID.fromString)
  implicit val instant: XStrDecoder[Instant] = str(Instant.parse)
  implicit val char: XStrDecoder[Char] = string.emap {
    case s if s.length == 1 => \/-(s(0))
    case s                  => -\/(s"text too long: $s")
  }
  implicit val symbol: XStrDecoder[Symbol] = str(Symbol(_))

  implicit val stringAttr: XStrDecoder[String @@ XAttribute] =
    string.map(Tag(_))
  implicit val booleanAttr: XStrDecoder[Boolean @@ XAttribute] =
    boolean.map(Tag(_))
  implicit val intAttr: XStrDecoder[Int @@ XAttribute] =
    int.map(Tag(_))
  implicit val longAttr: XStrDecoder[Long @@ XAttribute] =
    long.map(Tag(_))

  implicit val xcdata: XStrDecoder[XCdata] = {
    case XString(text) => \/-(XCdata(text))
  }

}

/** data types in the scala stdlib */
trait XStrDecoderStdlib {
  this: XStrDecoder.type =>

  implicit def either[
    A: XStrDecoder,
    B: XStrDecoder
  ]: XStrDecoder[Either[A, B]] = { x =>
    XStrDecoder[A]
      .fromXml(x)
      .map(Left(_))
      .orElse(
        XStrDecoder[B]
          .fromXml(x)
          .map(Right(_))
      )
  }

  import scala.concurrent.duration._
  implicit def finite: XStrDecoder[FiniteDuration] = long.emap { i =>
    if (i >= 0) \/-(i.millis)
    else -\/(s"got a negative number of milliseconds: $i")
  }
}

trait XStrDecoderRefined {
  this: XStrDecoder.type =>

  import eu.timepit.refined
  import eu.timepit.refined.api._

  implicit def aRefinedB[A: XStrDecoder, B](
    implicit V: Validate[A, B]
  ): XStrDecoder[A Refined B] =
    XStrDecoder[A].emap(refined.refineV(_).disjunction)

}
