// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz._, Scalaz._
import simulacrum._

@typeclass(generateAllOps = false)
trait XDecoder[A] { self =>
  def fromXml(x: XChildren): String \/ A
}
object XDecoder
    extends XDecoderScalaz1
    with XDecoderStdlib1
    with XDecoderStdlib2 {
  @inline def instance[A](f: XChildren => String \/ A): XDecoder[A] = f(_)

  object ops extends ToXDecoderOps {
    implicit class XDecoderOps(private val x: XChildren) extends AnyVal {
      def decode[A: XDecoder]: String \/ A = XDecoder[A].fromXml(x)
    }
  }

  // suppress long messages from the failure messages as they are never useful
  // and better to look at the source XML.
  def fail[A](expected: String, got: XNode): -\/[String] =
    -\/(s"expected $expected, got ${got.toString.take(200)}")

  // sometimes we need a disambiguating tag when decoding
  def tagged[A](name: String, delegate: XDecoder[A]): XDecoder[A] = {
    case x @ XChildren(ICons(XTag(XAtom(`name`), _, _, _), INil())) =>
      delegate.fromXml(x)
    case other =>
      XDecoder.fail(name, other)
  }

  import Isomorphism.<~>
  val iso: XDecoder <~> Kleisli[String \/ ?, XChildren, ?] = Kleisli.iso(
    λ[λ[a => (XChildren => String \/ a)] ~> XDecoder](instance(_)),
    λ[XDecoder ~> λ[a => (XChildren => String \/ a)]](_.fromXml)
  )
  implicit val monad: MonadError[XDecoder, String] = MonadError.fromIso(iso)
}

trait XDecoderScalaz1 {
  this: XDecoder.type =>

  implicit def disjunction[A: XDecoder, B: XDecoder]: XDecoder[A \/ B] = { x =>
    (XDecoder[A].fromXml(x), XDecoder[B].fromXml(x)) match {
      case (\/-(value), -\/(_)) => value.left[B].right[String]
      case (-\/(_), \/-(value)) => value.right[A].right[String]
      case (\/-(_), \/-(_))     => fail(s"only one branch to succeed", x)
      case (-\/(erl), -\/(err)) =>
        fail(s"one branch to succeed:\n$erl\n$err", x)
    }
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
