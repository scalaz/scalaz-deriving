// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz._, Scalaz._

@simulacrum.typeclass(generateAllOps = false)
trait XDecoder[A] { self =>
  import XDecoder.Out

  def fromXml(x: XChildren): Out[A]

  def map[B](f: A => B): XDecoder[B]                     = x => self.fromXml(x).map(f)
  def xmap[B](f: A => B, @unused g: B => A): XDecoder[B] = map(f)
  def emap[B](f: A => Out[B]): XDecoder[B]               = x => self.fromXml(x).flatMap(f)
}
object XDecoder
    extends XDecoderScalaz1
    with XDecoderStdlib1
    with XDecoderRefined
    with XDecoderStdlib2 {

  type Out[A] = String \/ A
  object ops extends ToXDecoderOps {
    implicit class XDecoderOps(private val x: XChildren) extends AnyVal {
      def decode[A: XDecoder]: Out[A] = XDecoder[A].fromXml(x)
    }
  }

  // DESNOTE(2018-05-03, SHalliday) suppress long messages from the failure
  // messages as they are never useful and better to look at the source XML.
  def fail[A](expected: String, got: XNode): -\/[String] =
    -\/(s"expected $expected, got ${got.toString.take(200)}")
}

trait XDecoderScalaz1 {
  this: XDecoder.type =>

  implicit def disjunction[A: XDecoder, B: XDecoder]: XDecoder[A \/ B] = { x =>
    XDecoder[A]
      .fromXml(x)
      .map(_.left[B])
      .orElse(
        XDecoder[B]
          .fromXml(x)
          .map(_.right[A])
      )
  }

  implicit def ilistStr[A: XStrDecoder]: XDecoder[IList[A]] = { xs =>
    xs.tree.traverse {
      case XTag(_, _, _, Maybe.Just(body)) =>
        XStrDecoder[A].fromXml(body)
      case other => fail(s"a single tag with a body", other.asChild)
    }
  }

  implicit def ilist[A: XDecoder]: XDecoder[IList[A]] = { xs =>
    xs.tree.traverse(x => XDecoder[A].fromXml(x.asChild))
  }

  implicit def nelStr[A: XStrDecoder]: XDecoder[NonEmptyList[A]] =
    ilistStr[A].emap { lst =>
      lst.toNel \/> "list was empty"
    }

  implicit def nel[A: XDecoder]: XDecoder[NonEmptyList[A]] =
    ilist[A].emap { lst =>
      lst.toNel \/> "list was empty"
    }

}

trait XDecoderScalaz2 {
  this: XDecoder.type =>

  // https://github.com/scalaz/scalaz/issues/1513 if scalaz had an FromFoldable
  // or Reducer to construct Foldable / Foldable1 from a List, it would go
  // here...
}

trait XDecoderStdlib1 {
  this: XDecoder.type =>

  implicit def either[A: XDecoder, B: XDecoder]: XDecoder[Either[A, B]] =
    disjunction[A, B].map(_.toEither)

  implicit def listStr[A: XStrDecoder]: XDecoder[List[A]] =
    ilistStr[A].map(_.toList)
  implicit def list[A: XDecoder]: XDecoder[List[A]] = ilist[A].map(_.toList)

  implicit def tuple2[A: XNodeDecoder, B: XNodeDecoder]: XDecoder[(A, B)] = {
    case XChildren(
        ICons(
          XTag(
            _,
            _,
            ICons(key @ XTag(XAtom("key"), _, _, _), value),
            _
          ),
          INil()
        )
        ) =>
      XNodeDecoder[A]
        .fromXml(key.asChild)
        .tuple(XNodeDecoder[B].fromXml(XChildren(value)))
    case other => fail(s"entry with key and value", other)
  }

  implicit def dict[A: XNodeDecoder, B: XNodeDecoder]: XDecoder[Map[A, B]] =
    list[(A, B)].map(_.toMap)

}

trait XDecoderStdlib2 {
  this: XDecoder.type =>

  implicit def cbfStr[T[_], A: XStrDecoder](
    implicit CBF: collection.generic.CanBuildFrom[Nothing, A, T[A]]
  ): XDecoder[T[A]] = listStr[A].map(_.to[T])

  implicit def cbf[T[_], A: XDecoder](
    implicit CBF: collection.generic.CanBuildFrom[Nothing, A, T[A]]
  ): XDecoder[T[A]] = list[A].map(_.to[T])

}

trait XDecoderRefined {
  this: XDecoder.type =>

  import eu.timepit.refined
  import refined.api._

  implicit def aRefinedB[A: XDecoder, B](
    implicit V: Validate[A, B]
  ): XDecoder[A Refined B] =
    XDecoder[A].emap(refined.refineV(_).disjunction)

}
