// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import java.util.UUID
import java.time.Instant

import scala.reflect.ClassTag
import scala.util.control.NonFatal

import scalaz._, Scalaz._

// DESNOTE(2017-12-21, SHalliday) for now we are being very strict on the inputs
// when decoding. We will probably want to ignore optional things rather than
// fail when we see something unexpected, e.g. when .traverse on a list looking
// for specific things.
@simulacrum.typeclass(generateAllOps = false)
trait XDecoder[A] { self =>
  import XDecoder.Out

  def fromXml(x: XNode): Out[A]

  def map[B](f: A => B): XDecoder[B]             = x => self.fromXml(x).map(f)
  def xmap[B](f: A => B, g: B => A): XDecoder[B] = map(f)
  def andThen[B](f: A => Out[B]): XDecoder[B]    = x => self.fromXml(x).flatMap(f)
}
object XDecoder extends XDecoderScalaz with XDecoderStdlib {

  type Out[A] = String \/ A
  object ops extends ToXDecoderOps {
    implicit class XDecoderOps(private val x: XNode) extends AnyVal {
      def decode[A: XDecoder]: Out[A] = XDecoder[A].fromXml(x)
    }
  }

  @inline def instance[A](f: PartialFunction[XNode, Out[A]]): XDecoder[A] =
    x =>
      f.applyOrElse(x, { _: XNode =>
        -\/(s"unexpected: $x")
      })
  // WORKAROUND https://github.com/scalaz/scalaz/issues/1590
  private def str[A](f: String => A)(implicit A: ClassTag[A]): XDecoder[A] =
    string.andThen { s =>
      try \/-(f(s))
      catch {
        case NonFatal(_) => -\/(s"expected $A in '$s'")
      }
    }

  implicit val string: XDecoder[String] = {
    case XString(text) => \/-(text)
    case other         => -\/(s"expected text, got $other")
  }

  implicit val boolean: XDecoder[Boolean] = str(_.toBoolean)
  implicit val short: XDecoder[Short]     = str(_.toShort)
  implicit val int: XDecoder[Int]         = str(_.toInt)
  implicit val long: XDecoder[Long]       = str(_.toLong)
  implicit val float: XDecoder[Float]     = str(_.toFloat)
  implicit val double: XDecoder[Double]   = str(_.toDouble)
  implicit val uuid: XDecoder[UUID]       = str(UUID.fromString)
  implicit val instant: XDecoder[Instant] = str(Instant.parse)
  implicit val char: XDecoder[Char] = string.andThen {
    case s if s.length == 1 => \/-(s(0))
    case s                  => -\/(s"text too long: $s")
  }
  implicit val symbol: XDecoder[Symbol] = str(Symbol(_))

  implicit val stringAttr: XDecoder[String @@ XAttribute] =
    string.map(Tag(_))

  implicit val xnode: XDecoder[XNode] = \/-(_)
  implicit val xcdata: XDecoder[XCdata] = instance {
    case XString(text) => \/-(XCdata(text))
  }

}

/** data types in the scala stdlib */
trait XDecoderStdlib {
  this: XDecoder.type =>

  import scala.concurrent.duration._

  implicit def option[A: XDecoder]: XDecoder[Option[A]] = {
    case XChildren(INil()) => \/-(None)
    case a                 => XDecoder[A].fromXml(a).map(Some(_))
  }
  implicit def optionAttr[A](
    implicit X: XDecoder[A @@ XAttribute]
  ): XDecoder[Option[A] @@ XAttribute] = {
    case XChildren(INil()) => \/-(Tag(None))
    case x                 => X.fromXml(x).map(a => Tag(Some(Tag.unwrap(a))))
  }

  implicit def either[A: XDecoder, B: XDecoder]: XDecoder[Either[A, B]] = { x =>
    XDecoder[A].fromXml(x).map(Left(_)) orElse XDecoder[B]
      .fromXml(x)
      .map(Right(_))
  }

  implicit def finite: XDecoder[FiniteDuration] = long.andThen { i =>
    if (i >= 0) \/-(i.millis)
    else -\/(s"got a negative number of milliseconds: $i")
  }

  private def ilist[A: XDecoder](key: XAtom): XDecoder[IList[A]] = instance {
    case XTag(`key`, INil(), v) => XDecoder[A].fromXml(v).map(IList.single)
    case XChildren(list) =>
      list.traverse {
        case XTag(`key`, INil(), v) => XDecoder[A].fromXml(v)
        case other                  => -\/(s"unexpected $other")
      }
  }

  private def dictEntry[K, V](
    implicit K: XDecoder[K],
    V: XDecoder[V]
  ): XDecoder[(K, V)] = instance {
    case XChildren(
        ICons(
          XTag(XString("key"), INil(), key),
          ICons(XTag(XString("value"), INil(), value), INil())
        )
        ) =>
      K.fromXml(key) tuple V.fromXml(value)
  }
  implicit def dict[K: XDecoder, V: XDecoder]: XDecoder[Map[K, V]] =
    ilist(XAtom("entry"))(dictEntry[K, V]).map(_.toList.toMap)

  implicit def cbf[T[_], A: XDecoder](
    implicit CBF: collection.generic.CanBuildFrom[Nothing, A, T[A]]
  ): XDecoder[T[A]] = ilist[A](XAtom("value")).map(_.toList.to)

}

trait XDecoderScalaz {
  this: XDecoder.type with XDecoderStdlib =>

  implicit def validated[
    A: XDecoder,
    B: XDecoder
  ]: XDecoder[Validation[A, B]] = instance {
    case XTag(XString("Failure"), INil(), a) =>
      XDecoder[A].fromXml(a).map(_.failure)
    case XTag(XString("Success"), INil(), b) =>
      XDecoder[B].fromXml(b).map(_.success)
  }

  implicit def nel[A: XDecoder]: XDecoder[NonEmptyList[A]] =
    cbf[List, A].andThen { stdlib =>
      stdlib.toNel \/> "list was empty"
    }

}
