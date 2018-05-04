// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package generic

import scalaz.{ ICons, INil }
import scalaz.Scalaz._

import shapeless._
import shapeless.labelled._

import XDecoder.{ fail, Out }

abstract class DerivedXDecoderTag[R] {
  private[generic] def from(x: XTag): Out[R]
}
object DerivedXDecoderTag {
  def gen[T, Repr](
    implicit
    G: LabelledGeneric.Aux[T, Repr],
    LER: Cached[Strict[DerivedXDecoderTag[Repr]]],
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

  implicit val cnil: DerivedXDecoderTag[CNil] = x =>
    s"no valid typehint tag in '$x'".left[CNil]
  implicit def ccons[K <: Symbol, A, T <: Coproduct](
    implicit
    K: Witness.Aux[K],
    LDI: Lazy[XDecoder[A]],
    DR: DerivedXDecoderTag[T]
  ): DerivedXDecoderTag[FieldType[K, A] :+: T] = { in =>
    val hint = XAtom(K.value.name)
    if (in.name == hint)
      LDI.value.fromXml(in.asChild).map(a => Inl(field[K](a)))
    else
      DR.from(in).map(a => Inr(a))
  }

  implicit def cconsStr[K <: Symbol, A, T <: Coproduct](
    implicit
    K: Witness.Aux[K],
    DI: XStrDecoder[A],
    DR: DerivedXDecoderTag[T]
  ): DerivedXDecoderTag[FieldType[K, A] :+: T] = { in =>
    val hint = XAtom(K.value.name)
    if (in.name == hint)
      in.body.cata(
        b => DI.fromXml(b).map(a => Inl(field[K](a))),
        fail("a body", in.asChild)
      )
    else
      DR.from(in).map(a => Inr(a))
  }

}
