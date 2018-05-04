// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz._, Scalaz._

@simulacrum.typeclass
trait XEncoder[A] { self =>
  def toXml(a: A): XChildren

  def contramap[B](f: B => A): XEncoder[B]               = b => self.toXml(f(b))
  def xmap[B](@unused f: A => B, g: B => A): XEncoder[B] = contramap(g)
}
object XEncoder
    extends XEncoderScalaz1
    with XEncoderStdlib1
    with XEncoderRefined
    with XEncoderScalaz2
    with XEncoderStdlib2

trait XEncoderScalaz1 {
  this: XEncoder.type =>

  implicit def ilistStr[A: XStrEncoder]: XEncoder[IList[A]] = { as =>
    XChildren(
      as.map(a => XTag(XAtom("value"), XStrEncoder[A].toXml(a)))
    )
  }

  implicit def ilist[A: XEncoder]: XEncoder[IList[A]] = { as =>
    XChildren(as.flatMap(a => XEncoder[A].toXml(a).tree))
  }

  implicit def disjunction[A: XEncoder, B: XEncoder]: XEncoder[A \/ B] = {
    case -\/(a) => XEncoder[A].toXml(a)
    case \/-(b) => XEncoder[B].toXml(b)
  }

  implicit def nelStr[A: XStrEncoder]: XEncoder[NonEmptyList[A]] =
    ilistStr[A].contramap(_.list)

  implicit def nel[A: XEncoder]: XEncoder[NonEmptyList[A]] =
    ilist[A].contramap(_.list)

}

trait XEncoderScalaz2 {
  this: XEncoder.type =>

  // Foldable derivers are not provided because they can pick up unexpected
  // instances for `F[_]` such as `Either[A, ?]` and can create `XEncoder`
  // instances where we really only want an `XStrEncoder`. Also, due to the lack
  // of a builder for decoding, these would be one-way anyway.

}

trait XEncoderStdlib1 {
  this: XEncoder.type =>

  implicit def either[A: XEncoder, B: XEncoder]: XEncoder[Either[A, B]] =
    disjunction[A, B].contramap(_.disjunction)

  implicit def listStr[A: XStrEncoder]: XEncoder[List[A]] =
    ilistStr[A].contramap(_.toIList)
  implicit def list[A: XEncoder]: XEncoder[List[A]] =
    ilist[A].contramap(_.toIList)

  implicit def tuple2[A: XNodeEncoder, B: XNodeEncoder]: XEncoder[(A, B)] = {
    case (a, b) =>
      val key = XNodeEncoder[A].toXml(a) match {
        case XChildren(ts)  => ts.map(_.copy(name = XAtom("key")))
        case s @ XString(_) => IList.single(XTag(XAtom("key"), s))
      }
      val value = XNodeEncoder[B].toXml(b) match {
        case XChildren(ts)  => ts.map(_.copy(name = XAtom("value")))
        case s @ XString(_) => IList.single(XTag(XAtom("value"), s))
      }
      XTag(XAtom("entry"), XChildren(key ::: value)).asChild
  }

  implicit def dict[A: XNodeEncoder, B: XNodeEncoder]: XEncoder[Map[A, B]] =
    list[(A, B)].contramap(_.toList)
}

trait XEncoderStdlib2 {
  this: XEncoder.type =>

  implicit def traversableStr[T[_], A: XStrEncoder](
    implicit T: T[A] <:< Traversable[A]
  ): XEncoder[T[A]] = listStr.contramap(_.toList)

  implicit def traversable[T[_], A: XEncoder](
    implicit T: T[A] <:< Traversable[A]
  ): XEncoder[T[A]] = list[A].contramap(_.toList)

}

trait XEncoderRefined {
  this: XEncoder.type =>

  import eu.timepit.refined.api.Refined

  implicit def aRefinedB[A: XEncoder, B]: XEncoder[A Refined B] =
    XEncoder[A].contramap(_.value)
}
