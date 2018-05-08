// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import java.util.UUID
import java.time.Instant

import scala.util.control.NonFatal

import shapeless.Typeable
import scalaz._, Scalaz._
import simulacrum._

@typeclass(generateAllOps = false)
trait XStrDecoder[A] { self =>
  def fromXml(x: XString): String \/ A
}
object XStrDecoder extends XStrDecoderScalaz with XStrDecoderStdlib {
  @inline def instance[A](f: XString => String \/ A): XStrDecoder[A] = f(_)

  object ops extends ToXStrDecoderOps {
    implicit class XStrDecoderOps(private val x: XString) extends AnyVal {
      def decode[A: XStrDecoder]: String \/ A = XStrDecoder[A].fromXml(x)
    }
  }

  import Isomorphism.<~>
  val iso: XStrDecoder <~> Kleisli[String \/ ?, XString, ?] = Kleisli.iso(
    λ[λ[a => (XString => String \/ a)] ~> XStrDecoder](instance(_)),
    λ[XStrDecoder ~> λ[a => (XString => String \/ a)]](_.fromXml)
  )
  implicit val monad: MonadError[XStrDecoder, String] = MonadError.fromIso(iso)

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

trait XStrDecoderScalaz {
  this: XStrDecoder.type =>

  implicit def disjunction[
    A: XStrDecoder,
    B: XStrDecoder
  ]: XStrDecoder[A \/ B] = { x =>
    (XStrDecoder[A].fromXml(x), XStrDecoder[B].fromXml(x)) match {
      case (\/-(value), -\/(_)) => value.left[B].right[String]
      case (-\/(_), \/-(value)) => value.right[A].right[String]
      case (\/-(_), \/-(_))     => s"unable to disambiguate '$x'".left
      case (-\/(erl), -\/(err)) =>
        s"both branches failed for '$x':\nLeft: $erl\nRight: $err".left
    }
  }
}

trait XStrDecoderStdlib {
  this: XStrDecoder.type =>

  implicit def either[
    A: XStrDecoder,
    B: XStrDecoder
  ]: XStrDecoder[Either[A, B]] = disjunction[A, B].map(_.toEither)

  import scala.concurrent.duration._
  implicit def finite: XStrDecoder[FiniteDuration] = long.emap { i =>
    if (i >= 0) \/-(i.millis)
    else -\/(s"got a negative number of milliseconds: $i")
  }
}
