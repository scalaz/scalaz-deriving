/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package xmlformat

import scalaz.*
import scalaz.Scalaz.*

@simulacrum.typeclass
trait XNodeDecoder[A] {
  def fromXml(x: XNode): String \/ A
}
object XNodeDecoder {
  implicit def fromTags[A](implicit X: XDecoder[A]): XNodeDecoder[A] = {
    case c @ XChildren(_) => X.fromXml(c)
    case other            => -\/(s"expected tag data but got $other")
  }

  implicit def fromText[A](implicit X: XStrDecoder[A]): XNodeDecoder[A] = {
    case s @ XString(_)                                         => X.fromXml(s)
    case XChildren(ICons(XTag(_, _, _, Maybe.Just(s)), INil())) => X.fromXml(s)
    case other => -\/(s"expected string data but got $other")
  }

  implicit val xnode: XNodeDecoder[XNode] = x => x.right[String]
}
