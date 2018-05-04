// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package generic

import scalaz.{
  @@,
  \/,
  Foldable1,
  ICons,
  INil,
  Maybe,
  Monoid,
  NonEmptyList,
  Semigroup
}
import scalaz.Scalaz._

import shapeless._
import shapeless.labelled._

import XDecoder.fail

/**
 * Product and coproduct decoder. The inverse of DerivedXEncoder.
 */
sealed trait DerivedXDecoder[A] {
  private[generic] def from(x: XTag): String \/ A
}
object DerivedXDecoder extends LowPriorityDerivedXDecoder1 {

  def gen[T, Repr](
    implicit
    G: LabelledGeneric.Aux[T, Repr],
    LER: Cached[Strict[DerivedXDecoder[Repr]]],
    T: Typeable[T]
  ): XDecoder[T] = {
    case XChildren(ICons(t, INil())) =>
      LER.value.value
        .from(t)
        .map(G.from)
        .leftMap(reason => s"${T.describe} -> $reason")

    case got =>
      fail("one tag", got)
        .leftMap(reason => s"${T.describe} -> $reason")
  }

  trait PXDecoder[R] extends DerivedXDecoder[R]
  trait CXDecoder[R] extends DerivedXDecoder[R]

  implicit final class NiceMatch[A](private val a: A) extends AnyVal {
    @inline final def switch[B](f: A => B): B = f(a)
  }

  implicit val hnil: PXDecoder[HNil] = _ => HNil.right[String]

  implicit def hconsAttr[K <: Symbol, A, T <: HList](
    implicit K: Witness.Aux[K],
    DV: XStrDecoder[A],
    DR: PXDecoder[T]
  ): PXDecoder[FieldType[K, A @@ XAttribute] :: T] = { in =>
    hconsAttrOptional[K, A, T](K, DV, DR).from(in).flatMap {
      case XAttribute(Some(head)) :: tail =>
        (field[K](XAttribute(head)) :: tail).right[String]
      case _ =>
        fail(s"attr '${K.value.name}'", in.asChild)
    }
  }

  implicit def hconsAttrOptional[K <: Symbol, A, T <: HList](
    implicit K: Witness.Aux[K],
    DV: XStrDecoder[A],
    DR: PXDecoder[T]
  ): PXDecoder[FieldType[K, Option[A] @@ XAttribute] :: T] = { in =>
    val key = XAtom(K.value.name)

    in.attrs
      .find(_.name == key)
      .switch {
        case None       => Option.empty[A].right[String]
        case Some(attr) => DV.fromXml(attr.value).map(Option(_))
      }
      .flatMap { head =>
        DR.from(in).map { tail =>
          field[K](XAttribute(head)) :: tail
        }
      }
  }

  implicit def hconsInlinedStr[K <: Symbol, A, T <: HList](
    implicit
    DV: XStrDecoder[A],
    DR: PXDecoder[T]
  ): PXDecoder[FieldType[K, A @@ XInlined] :: T] = { in =>
    hconsInlinedStrOptional[K, A, T](DV, DR).from(in).flatMap {
      case XInlined(Some(head)) :: tail =>
        (field[K](XInlined(head)) :: tail).right[String]
      case _ =>
        fail(s"a body", in.asChild)
    }
  }

  implicit def hconsInlinedStrOptional[K <: Symbol, A, T <: HList](
    implicit
    DV: XStrDecoder[A],
    DR: PXDecoder[T]
  ): PXDecoder[FieldType[K, Option[A] @@ XInlined] :: T] = { in =>
    in.body
      .cata(
        DV.fromXml(_).map(Option(_)),
        Option.empty[A].right[String]
      )
      .flatMap { head =>
        DR.from(in).map { tail =>
          field[K](XInlined(head)) :: tail
        }
      }
  }

  private[this] val typehint = XAtom("typehint")
  implicit val cnil: CXDecoder[CNil] = tag =>
    s"no valid typehint in '$tag'".left[CNil]
  implicit def ccons[K <: Symbol, A, T <: Coproduct](
    implicit
    K: Witness.Aux[K],
    LDI: Lazy[XDecoder[A]],
    DR: CXDecoder[T]
  ): CXDecoder[FieldType[K, A] :+: T] = { in =>
    val hint = XAttr(typehint, XAtom(K.value.name))

    if (in.attrs.element(hint))
      LDI.value.fromXml(in.asChild).map(a => Inl(field[K](a)))
    else
      DR.from(in).map(a => Inr(a))
  }

  implicit def cconsStr[K <: Symbol, A, T <: Coproduct](
    implicit
    K: Witness.Aux[K],
    DI: XStrDecoder[A],
    DR: CXDecoder[T]
  ): CXDecoder[FieldType[K, A] :+: T] = { in =>
    val hint = XAttr(typehint, XAtom(K.value.name))

    if (in.attrs.element(hint))
      in.body.cata(
        b => DI.fromXml(b).map(a => Inl(field[K](a))),
        fail("a body", in.asChild)
      )
    else
      DR.from(in).map(a => Inr(a))
  }

}

trait LowPriorityDerivedXDecoder1 extends LowPriorityDerivedXDecoder2 {
  this: DerivedXDecoder.type =>

  implicit def hconsInlinedMonoid[K <: Symbol, A, T <: HList](
    implicit M: Monoid[A],
    LDV: Lazy[XDecoder[A]],
    DR: PXDecoder[T]
  ): PXDecoder[FieldType[K, A @@ XInlined] :: T] = { in =>
    DR.from(in).map { tail =>
      // ignores errors, failure to decode anything gives Monoid.empty
      val head =
        in.children
          .flatMap(ts => LDV.value.fromXml(ts.asChild).toMaybe.toIList)
          .fold
      field[K](XInlined(head)) :: tail
    }
  }

}

trait LowPriorityDerivedXDecoder2 extends LowPriorityDerivedXDecoder3 {
  this: DerivedXDecoder.type =>

  implicit def hcons[K <: Symbol, A, T <: HList](
    implicit K: Witness.Aux[K],
    LDV: Lazy[XDecoder[A]],
    DR: PXDecoder[T]
  ): PXDecoder[FieldType[K, A] :: T] = { in =>
    // does not call hconsOptional as an optimisation
    val key      = XAtom(K.value.name)
    val matching = XChildren(in.children.filter(_.name == key))
    LDV.value.fromXml(matching).flatMap { head =>
      DR.from(in).map { tail =>
        field[K](head) :: tail
      }
    }
  }

  implicit def hconsOptional[K <: Symbol, A, T <: HList](
    implicit K: Witness.Aux[K],
    LDV: Lazy[XDecoder[A]],
    DR: PXDecoder[T]
  ): PXDecoder[FieldType[K, Option[A]] :: T] = { in =>
    val key      = XAtom(K.value.name)
    val matching = in.children.filter(_.name == key)

    {
      if (matching.isEmpty) Option.empty[A].right[String]
      else LDV.value.fromXml(XChildren(matching)).map(Some(_))
    }.flatMap { head =>
      DR.from(in).map { tail =>
        field[K](head) :: tail
      }
    }
  }

  implicit def hconsStr[K <: Symbol, A, T <: HList](
    implicit K: Witness.Aux[K],
    DV: XStrDecoder[A],
    DR: PXDecoder[T]
  ): PXDecoder[FieldType[K, A] :: T] = { in =>
    hconsStrOptional[K, A, T](K, DV, DR).from(in).flatMap {
      case head :: tail =>
        (head: Option[A]) match {
          case None =>
            fail(s"a tag named '${K.value.name}' with a body", in.asChild)
          case Some(value) => (field[K](value) :: tail).right[String]
        }
    }
  }

  implicit def hconsStrOptional[K <: Symbol, A, T <: HList](
    implicit K: Witness.Aux[K],
    DV: XStrDecoder[A],
    DR: PXDecoder[T]
  ): PXDecoder[FieldType[K, Option[A]] :: T] = { in =>
    val key = XAtom(K.value.name)

    in.children
      .filter(_.name == key)
      .switch {
        case ICons(XTag(_, _, _, Maybe.Just(body)), INil()) =>
          DV.fromXml(body).map(Option(_))
        case _ =>
          Option.empty[A].right[String]
      }
      .flatMap { head =>
        DR.from(in).map { tail =>
          field[K](head) :: tail
        }
      }
  }

  implicit def hconsInlinedSemigroup[K <: Symbol, A, T <: HList](
    implicit K: Witness.Aux[K],
    M: Semigroup[A],
    LDV: Lazy[XDecoder[A]],
    DR: PXDecoder[T]
  ): PXDecoder[FieldType[K, A @@ XInlined] :: T] = { in =>
    DR.from(in).flatMap { tail =>
      val (fails, goods) =
        in.children.map(ts => LDV.value.fromXml(ts.asChild)).separate

      goods.toNel match {
        case Some(head) =>
          (field[K](XInlined(Foldable1[NonEmptyList].fold1(head))) :: tail)
            .right[String]
        case None =>
          val messages = fails.intercalate("\n")
          s"${K.value.name}:\n$messages".left
      }
    }
  }

  // XDecoderTags must be inlined
  implicit def hconsInlinedTag[K <: Symbol, A, T <: HList](
    implicit K: Witness.Aux[K],
    LDV: Lazy[XDecoderTag[A]],
    DR: PXDecoder[T]
  ): PXDecoder[FieldType[K, A @@ XInlined] :: T] = { in =>
    val decoder = Lazy(LDV.value.asXDecoder)
    hconsInlined(K, decoder, DR).from(in)
  }

}

trait LowPriorityDerivedXDecoder3 extends LowPriorityDerivedXDecoder4 {
  this: DerivedXDecoder.type =>

  implicit def hconsInlined[K <: Symbol, A, T <: HList](
    implicit K: Witness.Aux[K],
    LDV: Lazy[XDecoder[A]],
    DR: PXDecoder[T]
  ): PXDecoder[FieldType[K, A @@ XInlined] :: T] = { in =>
    DR.from(in).flatMap { tail =>
      val (fails, goods) =
        in.children.map(ts => LDV.value.fromXml(ts.asChild)).separate

      goods.switch {
        case ICons(a, INil()) => (field[K](XInlined(a)) :: tail).right[String]
        case other if other.isEmpty =>
          val messages = fails.intercalate("\n")
          s"${K.value.name}:\n$messages".left
        case other =>
          fail(
            s"one (not ${other.length}) value for ${K.value.name}",
            in.asChild
          )
      }
    }
  }

}

trait LowPriorityDerivedXDecoder4 {
  this: DerivedXDecoder.type =>

  // WORKAROUND https://github.com/milessabin/shapeless/issues/309
  implicit def hconsHack[K <: Symbol, A, Z, T <: HList](
    implicit K: Witness.Aux[K],
    DV: XDecoder[A @@ Z],
    DR: PXDecoder[T]
  ): PXDecoder[FieldType[K, A @@ Z] :: T] = hcons(K, DV, DR)

}
