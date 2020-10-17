// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz._, Scalaz._
import simulacrum._

@typeclass(generateAllOps = false)
trait XDecoder[A]        { self =>
  def fromXml(x: XChildren): String \/ A

  // for performance
  final def xmap[B](f: A => B, @unused g: B => A): XDecoder[B] = map(f)
  final def map[B](f: A => B): XDecoder[B]                     =
    j => fromXml(j).map(f)
  final def emap[B](f: A => String \/ B): XDecoder[B]          =
    j => fromXml(j).flatMap(f)
}
object XDecoder
    extends XDecoderScalaz1
    with XDecoderRefined
    with XDecoderStdlib1
    with XDecoderScalaz2
    with XDecoderStdlib2 {
  object ops extends ToXDecoderOps {
    implicit class XDecoderOps(private val x: XChildren) extends AnyVal {
      def decode[A: XDecoder]: String \/ A = XDecoder[A].fromXml(x)
    }
    implicit class XTagDecoderOps(private val x: XTag)   extends AnyVal {
      def decode[A: XDecoder]: String \/ A = XDecoder[A].fromXml(x.asChild)

      // inefficient if there are more lookups than entries
      def findAttr(name: String): Maybe[XAttr]    =
        x.attrs.find(_.name == name).toMaybe
      def findChildren(name: String): IList[XTag] =
        x.children.filter(_.name == name)
    }
  }

  @inline def instance[A](f: XChildren => String \/ A): XDecoder[A] = f(_)
  private type Sig[a] = XChildren => String \/ a
  private val iso                                  = Kleisli.iso(
    λ[Sig ~> XDecoder](instance(_)),
    λ[XDecoder ~> Sig](_.fromXml)
  )
  implicit val monad: MonadError[XDecoder, String] = MonadError.fromIso(iso)

  // suppress long messages from the failure messages as they are never useful
  // and better to look at the source XML.
  def fail[A](expected: String, got: XNode): -\/[String] =
    -\/(s"expected $expected, got ${got.toString.take(200)}")

  // sometimes we need a disambiguating tag when decoding
  def tagged[A](name: String, delegate: XDecoder[A]): XDecoder[A] = {
    case x @ XChildren(ICons(XTag(`name`, _, _, _), INil())) =>
      delegate.fromXml(x)
    case XChildren(ICons(XTag(got, _, _, _), INil()))        =>
      -\/(s"expected tag '$name' but got '$got'")
    case other                                               =>
      XDecoder.fail(name, other)
  }

}

private[xmlformat] trait XDecoderScalaz1 {
  this: XDecoder.type =>

  implicit def disjunction[A: XDecoder, B: XDecoder]: XDecoder[A \/ B] = { x =>
    (XDecoder[A].fromXml(x), XDecoder[B].fromXml(x)) match {
      case (\/-(value), -\/(_)) => value.left[B].right[String]
      case (-\/(_), \/-(value)) => value.right[A].right[String]
      case (\/-(_), \/-(_))     => fail(s"only one branch to succeed", x)
      case (-\/(erl), -\/(err)) =>
        s"expected one branch to succeed, got:\nLeft: $erl\nRight: $err".left
    }
  }

  implicit def ilistStr[A: XStrDecoder]: XDecoder[IList[A]] = { xs =>
    xs.tree.traverseDisjunction {
      case XTag(_, _, _, Maybe.Just(body)) =>
        XStrDecoder[A].fromXml(body)
      case other                           => fail(s"a single tag with a body", other.asChild)
    }
  }

  implicit def ilist[A: XDecoder]: XDecoder[IList[A]] = { xs =>
    xs.tree.traverseDisjunction(x => XDecoder[A].fromXml(x.asChild))
  }

  implicit def nelStr[A: XStrDecoder]: XDecoder[NonEmptyList[A]] =
    ilistStr[A].emap { lst =>
      lst.toNel \/> "list was empty"
    }

  implicit def nel[A: XDecoder]: XDecoder[NonEmptyList[A]] =
    ilist[A].emap { lst =>
      lst.toNel \/> "list was empty"
    }

  implicit def tagged[A: XDecoder, Z]: XDecoder[A @@ Z] =
    XDecoder[A].map(Tag(_))

}

private[xmlformat] trait XDecoderScalaz2 {
  this: XDecoder.type =>

  // https://github.com/scalaz/scalaz/issues/1513 if scalaz had an FromFoldable
  // or Reducer to construct Foldable / Foldable1 from a List, it would go
  // here...
}

// WORKAROUND https://github.com/scala/bug/issues/10753
private[xmlformat] trait XDecoderRefined {
  this: XDecoder.type =>

  import eu.timepit.refined.refineV
  import eu.timepit.refined.api._

  implicit def refined[A: XDecoder, B](implicit
    V: Validate[A, B]
  ): XDecoder[A Refined B] =
    XDecoder[A].emap(refineV(_).toDisjunction)
}

private[xmlformat] trait XDecoderStdlib1 {
  this: XDecoder.type =>

  implicit def either[A: XDecoder, B: XDecoder]: XDecoder[Either[A, B]] =
    disjunction[A, B].map(_.toEither)

  implicit def listStr[A: XStrDecoder]: XDecoder[List[A]] =
    ilistStr[A].map(_.toList)
  implicit def list[A: XDecoder]: XDecoder[List[A]]       = ilist[A].map(_.toList)

  implicit def tuple2[A: XNodeDecoder, B: XNodeDecoder]: XDecoder[(A, B)] = {
    case XChildren(
          ICons(
            XTag(
              _,
              _,
              ICons(key @ XTag("key", _, _, _), value),
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

private[xmlformat] trait XDecoderStdlib2 {
  this: XDecoder.type =>

}
