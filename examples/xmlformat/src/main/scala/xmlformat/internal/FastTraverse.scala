// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat.internal

import scala.annotation.tailrec
import scalaz._

// WORKAROUND https://github.com/scalaz/scalaz/issues/1956
private[xmlformat] object FastTraverse {

  implicit final class IListExtras[A](private val self: IList[A])
      extends AnyVal {

    /**
     * Referentially transparent replacement for traverse, specialised to
     * disjunction.
     */
    def traverseDisjunction[E, B](f: A => E \/ B): E \/ IList[B] = {
      @tailrec def go(lst: IList[A], acc: IList[B]): E \/ IList[B] = lst match {
        case INil() => \/-(acc)
        case ICons(head, tail) =>
          f(head) match {
            case \/-(b)       => go(tail, b :: acc)
            case err @ -\/(_) => err
          }
      }
      go(self, IList.empty).map(_.reverse)
    }

    /**
     * Faster, but not a referentially transparent replacement for traverse
     * because map runs in reverse and therefore the *last* (not the first)
     * error may be reported.
     */
    def traverseDisjunctionFast[E, B](f: A => E \/ B): E \/ IList[B] = {
      val res = self.map { a =>
        f(a) match {
          case -\/(err) => return -\/(err) // scalafix:ok
          case \/-(j)   => j
        }
      }
      \/-(res)
    }
  }
}
