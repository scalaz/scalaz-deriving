// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat
package cord

import scala.annotation.tailrec

import scalaz._

// backport of scalaz.Cord from 7.3
sealed abstract class TCord {
  final def shows: String = {
    val sb = new StringBuilder
    TCord.appendTo(this, sb)
    sb.toString
  }

  /** Strict evaluation variant of Monoid.append */
  final def ++(o: TCord): TCord = TCord.Branch(this, o)
}
object TCord {
  private[cord] final case class Leaf(
    s: String
  ) extends TCord

  // Limiting the depth of a branch ensures we don't get stack overflows, at the
  // cost of forcing some intermediate strings.
  //
  // However, repeated monoidic appends produce large LEFT legs. A more
  // efficient solution may come later which reassociates, allowing us to tail
  // recurse down long RIGHT legs, and only use the rebalance for the shorter
  // LEFT legs.
  private[cord] final case class Branch private (
    leftDepth: Int,
    left: TCord,
    right: TCord
  ) extends TCord

  private[cord] object Branch {
    val max: Int = 100
    def apply(a: TCord, b: TCord): TCord = a match {
      case Leaf(_) =>
        Branch(1, a, b)
      case Branch(leftDepth, _, _) =>
        val branch = Branch(leftDepth + 1, a, b)
        if (leftDepth >= max)
          Leaf(branch.shows)
        else
          branch
    }
  }

  def apply(s: String): TCord =
    if (s.isEmpty) empty
    else Leaf(s)

  def apply(): TCord = empty
  val empty: TCord   = Leaf("")

  implicit val monoid: Monoid[TCord] = new Monoid[TCord] {
    def zero: TCord                           = empty
    def append(f1: TCord, f2: =>TCord): TCord = Branch(f1, f2)
  }

  implicit val equal: Equal[TCord] = Equal.equal((a, b) => a.shows == b.shows)

  @tailrec private def appendTo(c: TCord, sb: StringBuilder): Unit = c match {
    case Branch(_, a, b) =>
      appendTo_(a, sb) // not tail recursive, left legs need to be capped
      appendTo(b, sb)  // tail recursive, right legs can be arbitrarilly long
    case Leaf(s) =>
      val _ = sb.append(s)
  }
  // breaks out of the tail recursion
  private[this] def appendTo_(c: TCord, sb: StringBuilder): Unit =
    appendTo(c, sb)

}
