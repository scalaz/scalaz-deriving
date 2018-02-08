// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import java.lang.String

import scala.Predef.ArrowAssoc
import scala.collection.immutable.{ Map, Nil, Seq }
import scala.reflect.ClassTag
import scala.xml._

import scalaz.{ :+: => _, Coproduct => _, _ }
import scalaz.Scalaz._
import shapeless.{ :: => :*:, _ }
import shapeless.labelled._

trait DerivedEncoder[T] extends Encoder[T]

object DerivedEncoder {
  private def instance[A](f: A => NodeSeq): DerivedEncoder[A] =
    new DerivedEncoder[A] {
      override def toXml(a: A): NodeSeq = f(a)
    }

  def gen[T, Repr](
    implicit
    G: LabelledGeneric.Aux[T, Repr],
    LER: Cached[Strict[DerivedEncoder[Repr]]]
  ): DerivedEncoder[T] = instance { t =>
    LER.value.value.toXml(G.to(t))
  }

  implicit val hnil: DerivedEncoder[HNil] = instance { _ =>
    Group(Nil)
  }
  implicit def hcons[Key <: Symbol, Val, Remaining <: HList](
    implicit Key: Witness.Aux[Key],
    LEV: Lazy[Encoder[Val]],
    LER: DerivedEncoder[Remaining]
  ): DerivedEncoder[FieldType[Key, Val] :*: Remaining] =
    instance {
      case head :*: tail =>
        val entry = Encoder.el(Key.value.name, LEV.value.toXml(head))
        LER.toXml(tail) match {
          case g: Group => Group(entry :: g.nodes.toList)
        }
    }

  implicit val cnil: DerivedEncoder[CNil] = null
  implicit def ccons[Key <: Symbol, Instance, Remaining <: Coproduct](
    implicit
    Key: Witness.Aux[Key],
    LEI: Lazy[Encoder[Instance]],
    LER: DerivedEncoder[Remaining]
  ): DerivedEncoder[FieldType[Key, Instance] :+: Remaining] = instance {
    case Inl(ins) => Encoder.el(Key.value.name, LEI.value.toXml(ins))
    case Inr(rem) => LER.toXml(rem)
  }

}

trait DerivedDecoder[A] extends Decoder[A] {
  protected def fromXmlObject(xml: Map[String, NodeSeq]): Decoder.Decoded[A]
}

object DerivedDecoder {
  import DecoderUtils._

  private def instance[A](
    f: Map[String, NodeSeq] => Decoder.Decoded[A]
  ): DerivedDecoder[A] =
    new DerivedDecoder[A] {
      override def fromXml(xml: NodeSeq): Decoder.Decoded[A] = {
        xml match {
          case Group(els) => Right(els)
          case el: Elem   => Right(Seq(el))
          case other      => Left(unexpected(other))
        }
      }.flatMap { xml =>
        fromXmlObject(
          xml.collect {
            case el: Elem => el.label -> el.children
          }.toMap
        )
      }

      override def fromXmlObject(
        xml: Map[String, NodeSeq]
      ): Decoder.Decoded[A] = f(xml)
    }

  def gen[A, Repr](
    implicit
    G: LabelledGeneric.Aux[A, Repr],
    CR: Cached[Strict[DerivedDecoder[Repr]]],
    C: ClassTag[A]
  ): DerivedDecoder[A] = new DerivedDecoder[A] {
    override def fromXml(xml: NodeSeq): Decoder.Decoded[A] =
      CR.value.value
        .fromXml(xml)
        .map(G.from)
        .leftMap(
          _ |+| NonEmptyList(s"when decoding ${C}")
        )

    override def fromXmlObject(xml: Map[String, NodeSeq]): Decoder.Decoded[A] =
      throw new java.lang.IllegalStateException

  }

  implicit val hnil: DerivedDecoder[HNil] = instance { case _ => Right(HNil) }
  implicit def hcons[Key <: Symbol, Val, Remaining <: HList](
    implicit Key: Witness.Aux[Key],
    LV: Lazy[Decoder[Val]],
    DR: DerivedDecoder[Remaining]
  ): DerivedDecoder[FieldType[Key, Val] :*: Remaining] =
    instance { obj =>
      val key = Key.value.name
      // using Validated lets us accumulate all errors. If we want to
      // short-circuit and only report the first error, remove
      // .validation
      val head = obj
        .get(key)
        .toRight(failure(s"missing element $key"))
        .flatMap(LV.value.fromXml)
        .map(field[Key](_))
        .validation
      val remaining = DR.fromXmlObject(obj).validation

      (head |@| remaining).tupled.map {
        case (head, tail) => head :: tail
      }.toEither
    }

  implicit val cnil: DerivedDecoder[CNil] = instance { obj =>
    Left(NonEmptyList(s"no valid typehint in '${obj.keys.mkString}'"))
  }
  implicit def ccons[Key <: Symbol, Instance, Remaining <: Coproduct](
    implicit
    Key: Witness.Aux[Key],
    LI: Lazy[Decoder[Instance]],
    DR: DerivedDecoder[Remaining]
  ): DerivedDecoder[FieldType[Key, Instance] :+: Remaining] = instance { obj =>
    val key = Key.value.name
    obj.get(key) match {
      case Some(xml) => LI.value.fromXml(xml).map(a => Inl(field[Key](a)))
      case None      => DR.fromXmlObject(obj).map(a => Inr(a))
    }
  }

}
