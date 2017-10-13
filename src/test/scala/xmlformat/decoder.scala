// Copyright: 2017 https://gitlab.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package xmlformat

import java.lang.String
import java.math.{ BigDecimal => BD }

import scala.{
  Boolean,
  Char,
  Double,
  Either,
  Float,
  Int,
  Left,
  Long,
  None,
  Nothing,
  Option,
  PartialFunction,
  Right,
  Seq,
  Short,
  Some,
  StringContext,
  Symbol
}
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable._
import scala.concurrent.duration._
import scala.xml._

import scalaz._
import scalaz.Scalaz._

import simulacrum._

@typeclass(generateAllOps = false)
trait Decoder[A] { self =>
  def fromXml(xml: NodeSeq): Decoder.Decoded[A]

  final def map[B](f: A => B): Decoder[B] =
    new Decoder[B] {
      final override def fromXml(xml: NodeSeq): Decoder.Decoded[B] =
        self.fromXml(xml).map(f)
    }
  final def andThen[B](f: A => Decoder.Decoded[B]): Decoder[B] =
    new Decoder[B] {
      final override def fromXml(xml: NodeSeq): Decoder.Decoded[B] =
        self.fromXml(xml).flatMap(f)
    }
}

object DecoderUtils {
  def failure(msg: String): NonEmptyList[String] = NonEmptyList(msg)
  def unexpected[A](xml: NodeSeq): NonEmptyList[String] =
    failure(s"unexpected ${xml.getClass.getName}: ${xml}")

  // .child breaks out of the ADT, this safely returns us
  implicit class ElOps(el: Elem) {
    def children: NodeSeq = el.child match {
      case Seq(only) => only
      case many      => Group(many.toList)
    }
  }
}

object Decoder extends DecoderLowPriority {
  // https://github.com/mpilquist/simulacrum/issues/88
  object ops extends ToDecoderOps {
    implicit class DecoderOps(t: NodeSeq) {
      def fromXml[A](implicit d: Decoder[A]): Decoded[A] = d.fromXml(t)
      def decode[A: Decoder]                             = fromXml[A]
    }
  }

  import DecoderUtils._
  type Decoded[A] = Either[NonEmptyList[String], A]

  // PartialFunction saves boilerplate and gives us better runtime
  // safety, but at a small performance penalty.
  def instance[A](f: PartialFunction[NodeSeq, Decoded[A]]): Decoder[A] =
    new Decoder[A] {
      override def fromXml(xml: NodeSeq): Decoded[A] =
        f.applyOrElse(xml, { _: NodeSeq =>
          Left(unexpected(xml))
        })
    }

  // scala.xml uses anonymous classes not an ADT, so we can't match on
  // a StringLikeXml thing
  def strInstance[A](f: String => Option[A]): Decoder[A] = new Decoder[A] {
    override def fromXml(xml: NodeSeq): Decoded[A] =
      f(xml.text).toRight(unexpected(xml))
  }

  // AnyVal support
  implicit val invariant: InvariantFunctor[Decoder] =
    new InvariantFunctor[Decoder] {
      override def xmap[A, B](ma: Decoder[A],
                              f: A => B,
                              g: B => A): Decoder[B] =
        ma.map(f)
    }

  // because scala.xml has been designed so badly, it is possible to
  // request something reasonable but for type-based pattern-matching
  // to fail due to use of an unsealed anonymous class. Therefore
  // subtypes of NodeSeq must be manually handled.
  implicit val nodeSeq: Decoder[NodeSeq] = instance { case xml => Right(xml) }
  implicit val unparsed: Decoder[Unparsed] = instance {
    case u: Unparsed => Right(u)
    case xml         => Right(Unparsed(xml.toString))
  }
  implicit val pcdata: Decoder[PCData] = instance {
    case pc: PCData => Right(pc)
    case text: Text => Right(PCData(text.text)) // don't urlencode
    case xml        => Right(PCData(xml.toString))
  }

  implicit val boolean: Decoder[Boolean] = strInstance(_.parseBoolean.toOption)

  implicit val bd: Decoder[BD] = strInstance { str =>
    try { Some(new BD(str)) } catch {
      case iae: java.lang.IllegalArgumentException => None
    }
  }
  implicit val short: Decoder[Short]   = strInstance(_.parseShort.toOption)
  implicit val int: Decoder[Int]       = strInstance(_.parseInt.toOption)
  implicit val long: Decoder[Long]     = strInstance(_.parseLong.toOption)
  implicit val float: Decoder[Float]   = strInstance(_.parseFloat.toOption)
  implicit val double: Decoder[Double] = strInstance(_.parseDouble.toOption)

  implicit val string: Decoder[String] = strInstance(_.some)
  implicit val symbol: Decoder[Symbol] = string.map(s => Symbol(s))
  implicit val char: Decoder[Char] = string.andThen {
    case text if text.length == 1 => Right(text.charAt(0))
    case text                     => Left(failure(s"text too long: $text"))
  }

  implicit def option[T: Decoder]: Decoder[Option[T]] = instance {
    case xml if xml.text.isEmpty => Right(None)
    case other                   => Decoder[T].fromXml(other).map(_.some)
  }

  implicit def either[L: Decoder, R: Decoder]: Decoder[Either[L, R]] =
    instance {
      case el: Elem if el.label == "Left" =>
        Decoder[L].fromXml(el.children).map(Left(_))
      case el: Elem if el.label == "Right" =>
        Decoder[R].fromXml(el.children).map(Right(_))
    }

  private[xmlformat] def _list[A](
    label: String
  )(implicit D: Decoder[A]): Decoder[List[A]] =
    instance {
      case el: Elem if el.label == label => D.fromXml(el.children).map(List(_))
      case Group(els) =>
        els.toList.map {
          case el: Elem if el.label == label => D.fromXml(el.children)
          case other                         => Left(unexpected(other))
        }.sequence
    }

  implicit def list[A: Decoder]: Decoder[List[A]] =
    _list("value")

  implicit def nel[A: Decoder]: Decoder[NonEmptyList[A]] =
    list[A].andThen { stdlib =>
      IList.fromList(stdlib).toNel.toRight(failure("list was empty"))
    }

  implicit def validated[I: Decoder, V: Decoder]: Decoder[Validation[I, V]] =
    instance {
      case el: Elem if el.label == "Failure" =>
        Decoder[I].fromXml(el.children).map(Failure(_))
      case el: Elem if el.label == "Success" =>
        Decoder[V].fromXml(el.children).map(Success(_))
    }

  private def dictEntry[K, V](
    implicit K: Decoder[K],
    V: Decoder[V]
  ): Decoder[(K, V)] = instance {
    case Group(Seq(key: Elem, value: Elem))
        if key.label == "key" && value.label == "value" =>
      (K.fromXml(key) |@| V.fromXml(value)).tupled
  }

  implicit def dict[K: Decoder, V: Decoder]: Decoder[Map[K, V]] =
    _list("entry")(dictEntry[K, V]).map(_.toMap)

  implicit val finite: Decoder[FiniteDuration] =
    long.map(_.millis)

}

// low priority because they are expensive
trait DecoderLowPriority {
  this: Decoder.type =>

  implicit def cbf[T[_], A: Decoder](
    implicit CBF: CanBuildFrom[Nothing, A, T[A]]
  ): Decoder[T[A]] = _list[A]("value").map(_.to)

}
