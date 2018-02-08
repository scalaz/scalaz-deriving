// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz._

@simulacrum.typeclass
trait XEncoder[A] { self =>
  def toXml(a: A): XNode

  def contramap[B](f: B => A): XEncoder[B]       = b => self.toXml(f(b))
  def xmap[B](f: A => B, g: B => A): XEncoder[B] = contramap(g)
}
object XEncoder extends XEncoderScalaz with XEncoderStdlib {

  // JVM data types
  implicit val boolean: XEncoder[Boolean]           = a => XAtom(a.toString)
  implicit val short: XEncoder[Short]               = a => XAtom(a.toString)
  implicit val int: XEncoder[Int]                   = a => XAtom(a.toString)
  implicit val long: XEncoder[Long]                 = a => XAtom(a.toString)
  implicit val float: XEncoder[Float]               = a => XAtom(a.toString)
  implicit val double: XEncoder[Double]             = a => XAtom(a.toString)
  implicit val uuid: XEncoder[java.util.UUID]       = a => XAtom(a.toString)
  implicit val instant: XEncoder[java.time.Instant] = a => XAtom(a.toString)
  implicit val string: XEncoder[String]             = s => XText(s)
  implicit val char: XEncoder[Char]                 = string.contramap(_.toString)
  implicit val symbol: XEncoder[Symbol]             = string.contramap(_.name)

  implicit val stringAttr: XEncoder[String @@ XAttribute] =
    s => XText(Tag.unwrap(s))

  // trivial
  implicit val xnode: XEncoder[XNode]   = identity
  implicit val xcdata: XEncoder[XCdata] = identity
}

/** data types in the scala stdlib */
trait XEncoderStdlib {
  this: XEncoder.type =>

  import scala.concurrent.duration.FiniteDuration

  implicit def option[A: XEncoder]: XEncoder[Option[A]] = {
    case Some(a) => XEncoder[A].toXml(a)
    case None    => XChildren(IList.empty)
  }
  implicit def optionAttr[A](
    implicit X: XEncoder[A @@ XAttribute]
  ): XEncoder[Option[A] @@ XAttribute] = { t =>
    Tag.unwrap(t) match {
      case Some(a) => X.toXml(Tag(a))
      case None    => XChildren(IList.empty)
    }
  }

  implicit def either[A: XEncoder, B: XEncoder]: XEncoder[Either[A, B]] = {
    case Left(a)  => XEncoder[A].toXml(a)
    case Right(b) => XEncoder[B].toXml(b)
  }

  implicit def finite: XEncoder[FiniteDuration] = long.contramap(_.toMillis)

  private def ilist[A: XEncoder](key: XAtom): XEncoder[IList[A]] = { t =>
    XChildren(t.map { s =>
      XTag(key, IList.empty, XEncoder[A].toXml(s).asContent)
    })
  }

  private def dictEntry[K: XEncoder, V: XEncoder]: XEncoder[(K, V)] = {
    case (k, v) =>
      XChildren(
        IList(
          XTag(XAtom("key"), IList.empty, XEncoder[K].toXml(k).asContent),
          XTag(XAtom("value"), IList.empty, XEncoder[V].toXml(v).asContent)
        )
      )
  }
  implicit def dict[K: XEncoder, V: XEncoder]: XEncoder[Map[K, V]] =
    ilist(XAtom("entry"))(dictEntry[K, V])
      .contramap(m => IList.fromList(m.toList))

  implicit def traversable[T[_], A: XEncoder](
    implicit T: T[A] <:< Traversable[A]
  ): XEncoder[T[A]] =
    ilist[A](XAtom("value")).contramap(t => IList.fromList(T(t).toList))

}

trait XEncoderScalaz {
  this: XEncoderStdlib =>

  implicit def validated[A: XEncoder, B: XEncoder]: XEncoder[Validation[A, B]] =
    _.fold(
      a => XTag(XAtom("Invalid"), IList.empty, XEncoder[A].toXml(a).asContent),
      b => XTag(XAtom("Valid"), IList.empty, XEncoder[B].toXml(b).asContent)
    )

  implicit def nel[A: XEncoder]: XEncoder[NonEmptyList[A]] =
    traversable[List, A].contramap(_.list.toList)
}
