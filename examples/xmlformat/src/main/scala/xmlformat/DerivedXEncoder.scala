// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz.{ -\/, @@, \/, \/-, IList }
import scalaz.Scalaz._

import shapeless.{ :: => :*:, _ }
import shapeless.labelled._

/** Product and coproduct encoder. */
sealed trait DerivedXEncoder[R] {
  private[xmlformat] def to(r: R): IList[XAttr \/ XTag] \/ XTag
}
object DerivedXEncoder extends LowPriorityDerivedXEncoder {
  def gen[T, Repr](
    implicit
    T: Typeable[T],
    G: LabelledGeneric.Aux[T, Repr],
    LER: Cached[Strict[DerivedXEncoder[Repr]]]
  ): XEncoder[T] = { t =>
    LER.value.value.to(G.to(t)) match {
      case -\/(product) =>
        val (attrs, tags) = product.separate
        if (attrs.isEmpty)
          XChildren(tags)
        else
          XTag(XAtom(T.describe), attrs, XChildren(tags))
      case \/-(coproduct) => coproduct
    }
  }

  trait PXEncoder[R] extends DerivedXEncoder[R] {
    def to(r: R): IList[XAttr \/ XTag] \/ XTag = -\/(product(r))
    def product(r: R): IList[XAttr \/ XTag]
  }
  trait CXEncoder[R] extends DerivedXEncoder[R] {
    def to(r: R): IList[XAttr \/ XTag] \/ XTag = \/-(coproduct(r))
    def coproduct(r: R): XTag
  }

  implicit val hnil: PXEncoder[HNil] = _ => IList.empty[XAttr \/ XTag]
  implicit def hconsAttr[Key <: Symbol, Value, Remaining <: HList](
    implicit
    Key: Witness.Aux[Key],
    EV: XEncoder[Value @@ XAttribute],
    ER: PXEncoder[Remaining]
  ): PXEncoder[FieldType[Key, Value @@ XAttribute] :*: Remaining] = {
    case head :*: tail =>
      (ER.product(tail), EV.toXml(head)) match {
        case (rest, contents: XString) =>
          val name = XAtom(Key.value.name)
          -\/(XAttr(name, contents)) :: rest
        case (rest, _) => rest // ignores non-text attributes
      }
  }

  implicit val cnil: CXEncoder[CNil] = _ => sys.error("bad coproduct")
  implicit def ccons[Name <: Symbol, Instance, Remaining <: Coproduct](
    implicit
    Name: Witness.Aux[Name],
    LEI: Lazy[XEncoder[Instance]],
    ER: CXEncoder[Remaining]
  ): CXEncoder[FieldType[Name, Instance] :+: Remaining] = {
    case Inl(ins) =>
      val name = XAtom(Name.value.name)

      LEI.value.toXml(ins) match {
        case t @ XTag(`name`, attrs, _) if attrs.nonEmpty => t
        case x                                            => XTag(name, IList.empty, x.asContent)
      }

    case Inr(rem) => ER.coproduct(rem)
  }

}

trait LowPriorityDerivedXEncoder {
  this: DerivedXEncoder.type =>

  implicit def hcons[Key <: Symbol, Value, Remaining <: HList](
    implicit Key: Witness.Aux[Key],
    LEV: Lazy[XEncoder[Value]],
    ER: PXEncoder[Remaining]
  ): PXEncoder[FieldType[Key, Value] :*: Remaining] = {
    case head :*: tail =>
      val name     = XAtom(Key.value.name)
      val contents = LEV.value.toXml(head)
      \/-(XTag(name, IList.empty, contents.asContent)) :: ER.product(tail)
  }
}
