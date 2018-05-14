// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package generic

import scalaz.{ ICons, INil }

import shapeless._
import shapeless.labelled._

trait DerivedXEncoderTag[A] extends XEncoderTag[A]
object DerivedXEncoderTag {
  def gen[A, Repr](
    implicit
    G: LabelledGeneric.Aux[A, Repr],
    LER: Cached[Strict[DerivedXEncoderTag[Repr]]]
  ): XEncoderTag[A] = { t =>
    LER.value.value.toXTag(G.to(t))
  }

  implicit val cnil: DerivedXEncoderTag[CNil] = _ => sys.error("bad coproduct")
  implicit def ccons[K <: Symbol, A, T <: Coproduct](
    implicit
    K: Witness.Aux[K],
    LEI: Lazy[XNodeEncoder[A]],
    ER: DerivedXEncoderTag[T]
  ): DerivedXEncoderTag[FieldType[K, A] :+: T] = {
    case Inl(ins) =>
      LEI.value.toXml(ins) match {
        case XChildren(ICons(t @ XTag(_, _, _, _), INil())) =>
          t.copy(name = K.value.name)
        case c => XTag(K.value.name, c)
      }
    case Inr(rem) => ER.toXTag(rem)
  }

}
