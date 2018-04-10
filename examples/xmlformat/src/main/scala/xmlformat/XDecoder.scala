// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import java.util.UUID
import java.time.Instant

import scala.util.control.NonFatal

import shapeless.Typeable
import scalaz._, Scalaz._

@simulacrum.typeclass(generateAllOps = false)
trait XDecoder[A] { self =>
  import XDecoder.Out

  def fromXml(x: XNode): Out[A]

  def map[B](f: A => B): XDecoder[B]                     = x => self.fromXml(x).map(f)
  def xmap[B](f: A => B, @unused g: B => A): XDecoder[B] = map(f)
  def emap[B](f: A => Out[B]): XDecoder[B]               = x => self.fromXml(x).flatMap(f)
}
object XDecoder extends XDecoderScalaz with XDecoderStdlib {

  type Out[A] = String \/ A
  object ops extends ToXDecoderOps {
    implicit class XDecoderOps(private val x: XNode) extends AnyVal {
      def decode[A: XDecoder]: Out[A] = XDecoder[A].fromXml(x)
    }
  }

  // because I'm lazy... would be best as a macro
  @inline def instance[A](f: PartialFunction[XNode, Out[A]]): XDecoder[A] =
    x =>
      f.applyOrElse(x, { _: XNode =>
        -\/(s"unexpected: ${x.toString.take(100)}")
      })
  // WORKAROUND https://github.com/scalaz/scalaz/issues/1590
  private def str[A](f: String => A)(implicit A: Typeable[A]): XDecoder[A] =
    string.emap { s =>
      try \/-(f(s))
      catch {
        case NonFatal(_) => -\/(s"expected ${A.describe} in '$s'")
      }
    }

  implicit val string: XDecoder[String] = {
    case XString(text) => \/-(text.trim)
    case other         => -\/(s"expected text, got ${other.toString.take(100)}")
  }

  implicit val boolean: XDecoder[Boolean] = str(_.toBoolean)
  implicit val short: XDecoder[Short]     = str(_.toShort)
  implicit val int: XDecoder[Int]         = str(_.toInt)
  implicit val long: XDecoder[Long]       = str(_.toLong)
  implicit val float: XDecoder[Float]     = str(_.toFloat)
  implicit val double: XDecoder[Double]   = str(_.toDouble)
  implicit val uuid: XDecoder[UUID]       = str(UUID.fromString)
  implicit val instant: XDecoder[Instant] = str(Instant.parse)
  implicit val char: XDecoder[Char] = string.emap {
    case s if s.length == 1 => \/-(s(0))
    case s                  => -\/(s"text too long: $s")
  }
  implicit val symbol: XDecoder[Symbol] = str(Symbol(_))

  implicit val stringAttr: XDecoder[String @@ XAttribute] =
    string.map(Tag(_))
  implicit val booleanAttr: XDecoder[Boolean @@ XAttribute] =
    boolean.map(Tag(_))
  implicit val intAttr: XDecoder[Int @@ XAttribute] =
    int.map(Tag(_))
  implicit val longAttr: XDecoder[Long @@ XAttribute] =
    long.map(Tag(_))

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
    XDecoder[A]
      .fromXml(x)
      .map(Left(_))
      .orElse(
        XDecoder[B]
          .fromXml(x)
          .map(Right(_))
      )
  }

  implicit def finite: XDecoder[FiniteDuration] = long.emap { i =>
    if (i >= 0) \/-(i.millis)
    else -\/(s"got a negative number of milliseconds: $i")
  }

  // first try decoding from the body, then the children
  def fromXTagContent[A](tag: XTag)(implicit A: XDecoder[A]): String \/ A =
    tag.body.|> {
      case Maybe.Just(body) => A.fromXml(body)
      case Maybe.Empty()    => -\/("no body")
    }.orElse(A.fromXml(XChildren(tag.children)))

  private def ilist[A: XDecoder](key: XAtom): XDecoder[IList[A]] = instance {
    case t @ XTag(`key`, _, _, _) => fromXTagContent[A](t).map(IList.single)
    case XChildren(list) =>
      list.traverse {
        case t @ XTag(`key`, _, _, _) => fromXTagContent[A](t)
        case other =>
          -\/(s"expected XTag($key, [], _) got ${other.toString.take(100)}...")
      }
  }

  // variation of list encoding where keys are the values (since this is opt-in
  // we could do it without without checking the `key`)
  private def ilistInline[A: XDecoder]: XDecoder[IList[A]] = instance {
    case t @ XTag(_, _, _, _) => XDecoder[A].fromXml(t).map(IList.single)
    case XChildren(list) =>
      list.traverse {
        case t @ XTag(_, _, _, _) => XDecoder[A].fromXml(t)
        case other =>
          -\/(s"expected XTag got ${other.toString.take(100)}...")
      }
  }

  private def dictEntry[K, V](
    implicit K: XDecoder[K],
    V: XDecoder[V]
  ): XDecoder[(K, V)] = instance {
    case XChildren(
        ICons(
          k @ XTag(XString("key"), _, _, _),
          ICons(v @ XTag(XString("value"), _, _, _), INil())
        )
        ) =>
      fromXTagContent[K](k).tuple(fromXTagContent[V](v))
  }
  implicit def dict[K: XDecoder, V: XDecoder]: XDecoder[Map[K, V]] =
    ilist(XAtom("entry"))(dictEntry[K, V]).map(_.toList.toMap)

  implicit def cbf[T[_], A: XDecoder](
    implicit CBF: collection.generic.CanBuildFrom[Nothing, A, T[A]]
  ): XDecoder[T[A]] = ilist[A](XAtom("value")).map(_.toList.to)

  implicit def cbfInline[T[_], A: XDecoder: Typeable](
    implicit CBF: collection.generic.CanBuildFrom[Nothing, A, T[A]]
  ): XDecoder[T[A] @@ XInlinedList] =
    ilistInline[A].map(il => XInlinedList(il.toList.to))

}

trait XDecoderScalaz {
  this: XDecoder.type with XDecoderStdlib =>

  implicit def validated[A: XDecoder, B: XDecoder]: XDecoder[Validation[A, B]] =
    instance {
      case t @ XTag(XString("Failure"), _, _, _) =>
        fromXTagContent[A](t).map(_.failure)
      case t @ XTag(XString("Success"), _, _, _) =>
        fromXTagContent[B](t).map(_.success)
    }

  implicit def nel[A: XDecoder]: XDecoder[NonEmptyList[A]] =
    cbf[List, A].emap { stdlib =>
      stdlib.toNel \/> "list was empty"
    }

  implicit def nelInline[
    A: XDecoder: Typeable
  ]: XDecoder[NonEmptyList[A] @@ XInlinedList] =
    cbfInline[List, A].emap { stdlib =>
      XInlinedList
        .unwrap(stdlib)
        .toNel
        .map(XInlinedList(_)) \/> "list was empty"
    }

}
