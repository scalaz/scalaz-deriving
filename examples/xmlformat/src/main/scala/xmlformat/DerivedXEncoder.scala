// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz._, Scalaz._

import shapeless._
import shapeless.labelled._

import shapeless.{ :+:, Coproduct }

/** Product and coproduct encoder. */
sealed trait DerivedXEncoder[R] {
  private[xmlformat] def to(r: R): IList[XAttr \/ (XTag \/ XString)] \/ XTag
}
object DerivedXEncoder
    extends LowPriorityDerivedXEncoder1
    with LowPriorityDerivedXEncoder2
    with LowPriorityDerivedXEncoder3 {
  def gen[T, R](
    implicit
    T: Typeable[T],
    G: LabelledGeneric.Aux[T, R],
    LER: Cached[Strict[DerivedXEncoder[R]]]
  ): XEncoder[T] = { t =>
    LER.value.value.to(G.to(t)) match {
      case -\/(product) =>
        val (attrs, content) = product.separate
        val (children, body) = content.separate
        if (attrs.isEmpty && body.isEmpty)
          XChildren(children)
        else {
          XTag(XAtom(T.describe), attrs, children, body.fold1Opt.toMaybe)
        }
      case \/-(coproduct) => coproduct
    }
  }

  trait PXEncoder[R] extends DerivedXEncoder[R] {
    def to(r: R): IList[XAttr \/ (XTag \/ XString)] \/ XTag = -\/(product(r))
    def product(r: R): IList[XAttr \/ (XTag \/ XString)]
  }
  trait CXEncoder[R] extends DerivedXEncoder[R] {
    def to(r: R): IList[XAttr \/ (XTag \/ XString)] \/ XTag = \/-(coproduct(r))
    def coproduct(r: R): XTag
  }

  implicit val hnil: PXEncoder[HNil] = _ =>
    IList.empty[XAttr \/ (XTag \/ XString)]
  implicit def hconsAttr[K <: Symbol, A, AS <: HList](
    implicit
    K: Witness.Aux[K],
    EV: XEncoder[A @@ XAttribute],
    ER: PXEncoder[AS]
  ): PXEncoder[FieldType[K, A @@ XAttribute] :: AS] = {
    case head :: tail =>
      (ER.product(tail), EV.toXml(head)) match {
        case (rest, contents: XString) =>
          val name = XAtom(K.value.name)
          -\/(XAttr(name, contents)) :: rest
        case (rest, _) => rest // ignores non-text attributes
      }
  }

  implicit def hconsInlinedField[K <: Symbol, A, AS <: HList](
    implicit
    K: Witness.Aux[K],
    LEV: Lazy[XEncoder[A]],
    ER: PXEncoder[AS]
  ): PXEncoder[FieldType[K, A @@ XInlinedField] :: AS] = {
    case head :: tail =>
      // we can't check at compiletime that `head` decodes to an element with
      // the same name as K, so we choose to ignore the XInlinedField marker
      // at runtime if it does not match. It is only a "hint" afterall.
      val name = XAtom(K.value.name)
      val entries =
        LEV.value.toXml(XInlinedField.unwrap(head)) match {
          case t @ XTag(`name`, _, _, _) => IList.single(t)
          case t @ XTag(_, _, _, _)      => IList.single(XTag(name, t))
          case XChildren(children) =>
            children.traverse {
              case t @ XTag(`name`, _, _, _) => t.right[XNode]
              case other                     => other.left[XTag]
            } match {
              case \/-(lst) => lst
              case -\/(_) =>
                IList.single(XTag(name, XChildren(children)))
            }
          case body @ XString(_) =>
            IList.single(XTag(name, body))
        }
      entries.map(_.left[XString].right[XAttr]) ::: ER.product(tail)
  }

  // this should really return String @@ XInlinedContent but that would not
  // allow us to use things that contrmap via string.
  implicit def hconsInlinedContent[K <: Symbol, A, AS <: HList](
    implicit
    LEV: Lazy[XEncoder[A]],
    ER: PXEncoder[AS]
  ): PXEncoder[FieldType[K, A @@ XInlinedContent] :: AS] = {
    case head :: tail =>
      val value: IList[XAttr \/ (XTag \/ XString)] =
        LEV.value.toXml(XInlinedContent.unwrap(head)) match {
          case str @ XString(_)     => IList.single(\/-(\/-(str)))
          case t @ XTag(_, _, _, _) => IList.single(\/-(-\/(t)))
          case XChildren(c)         => c.map(_.left[XString].right[XAttr])
        }
      value ::: ER.product(tail)
  }

  implicit val cnil: CXEncoder[CNil] = _ => sys.error("bad coproduct")
  implicit def ccons[K <: Symbol, A, AS <: Coproduct](
    implicit
    K: Witness.Aux[K],
    LEI: Lazy[XEncoder[A]],
    ER: CXEncoder[AS]
  ): CXEncoder[FieldType[K, A] :+: AS] = {
    case Inl(ins) =>
      val name = XAtom(K.value.name)

      LEI.value.toXml(ins) match {
        case t @ XTag(`name`, ICons(_, _), _, _) => t
        case t @ XTag(_, _, _, _)                => XTag(name, t)
        case c @ XChildren(_)                    => XTag(name, c)
        case s @ XString(_)                      => XTag(name, s)
      }

    case Inr(rem) => ER.coproduct(rem)
  }

}

trait LowPriorityDerivedXEncoder1 {
  this: DerivedXEncoder.type =>

  // see comments in DerivedXDecoder
  implicit def hconsInlinedList[K <: Symbol, A, AS <: HList](
    implicit
    K: Witness.Aux[K],
    EV: XEncoder[A @@ XInlinedList],
    ER: PXEncoder[AS]
  ): PXEncoder[
    FieldType[K, A @@ XInlinedList @@ XInlinedField] :: AS
  ] =
    hconsInlinedField(K, Lazy(EV), ER)

}

trait LowPriorityDerivedXEncoder2 {
  this: DerivedXEncoder.type =>

  implicit def hcons[K <: Symbol, A, AS <: HList](
    implicit K: Witness.Aux[K],
    LEV: Lazy[XEncoder[A]],
    ER: PXEncoder[AS]
  ): PXEncoder[FieldType[K, A] :: AS] = {
    case head :: tail =>
      val name = XAtom(K.value.name)
      val tag  = XTag(name, LEV.value.toXml(head))
      \/-(-\/(tag)) :: ER.product(tail)
  }
}

trait LowPriorityDerivedXEncoder3 {
  this: DerivedXEncoder.type =>

  // WORKAROUND https://github.com/milessabin/shapeless/issues/309
  implicit def hconsHack[K <: Symbol, A, T, AS <: HList](
    implicit K: Witness.Aux[K],
    LEV: Lazy[XEncoder[A @@ T]],
    ER: PXEncoder[AS]
  ): PXEncoder[FieldType[K, A @@ T] :: AS] = hcons(K, LEV, ER)
}
