// Copyright: 2017 - 2021 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

@simulacrum.typeclass
trait XNodeEncoder[A] { self =>
  def toXml(a: A): XNode
}
object XNodeEncoder   {
  implicit def fromTags[A](implicit X: XEncoder[A]): XNodeEncoder[A]    =
    X.toXml(_)
  implicit def fromText[A](implicit X: XStrEncoder[A]): XNodeEncoder[A] =
    X.toXml(_)

  implicit val xnode: XNodeEncoder[XNode] = identity
}
