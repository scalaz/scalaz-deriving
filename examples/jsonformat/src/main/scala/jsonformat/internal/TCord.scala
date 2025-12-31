/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package jsonformat.internal

import scala.annotation.tailrec
import scalaz.*

// backport of scalaz.Cord from 7.3
private[jsonformat] sealed abstract class TCord {
  override final def toString: String = shows

  final def reset: TCord = TCord.Leaf(shows)

  final def shows: String =
    this match {
      case TCord.Leaf(str) => str
      case _               =>
        val sb = new StringBuilder
        TCord.unsafeAppendTo(this, sb)
        sb.toString
    }
  final def ::(o: TCord): TCord = TCord.Branch(o, this)
  final def ++(o: TCord): TCord = TCord.Branch(this, o)
}
private[jsonformat] object TCord {
  implicit def fromString(s: String): TCord = apply(s)
  def apply(s: String): TCord = Leaf.apply(s)
  def apply(): TCord = Leaf.Empty

  private[internal] final class Leaf private (
    val s: String
  ) extends TCord
  private[internal] object Leaf {
    val Empty: Leaf = new Leaf("")
    def apply(s: String): Leaf =
      if (s.isEmpty) Empty
      else new Leaf(s)
    def unapply(l: Leaf): Some[String] = Some(l.s)
  }

  private[internal] final class Branch private (
    val leftDepth: Int,
    val left: TCord,
    val right: TCord
  ) extends TCord
  private[internal] object Branch {
    val max: Int = 100
    def apply(a: TCord, b: TCord): TCord =
      if (a.eq(Leaf.Empty)) b
      else if (b.eq(Leaf.Empty)) a
      else
        a match {
          case _: Leaf =>
            new Branch(1, a, b)
          case a: Branch =>
            val branch = new Branch(a.leftDepth + 1, a, b)
            if (a.leftDepth >= max)
              branch.reset
            else
              branch
        }
    def unapply(b: Branch): Some[(Int, TCord, TCord)] =
      Some((b.leftDepth, b.left, b.right))
  }

  implicit val monoid: Monoid[TCord] = new Monoid[TCord] {
    def zero: TCord = Leaf.Empty
    def append(f1: TCord, f2: =>TCord): TCord = Branch(f1, f2)
  }

  @tailrec private def unsafeAppendTo(c: TCord, sb: StringBuilder): Unit =
    c match {
      case Branch(_, a, b) =>
        unsafeAppendTo_(a, sb)
        unsafeAppendTo(b, sb)
      case Leaf(s) =>
        val _ = sb.append(s)
    }
  private[this] def unsafeAppendTo_(c: TCord, sb: StringBuilder): Unit =
    unsafeAppendTo(c, sb)

}
