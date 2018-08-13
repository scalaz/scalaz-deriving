// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat.internal

import scalaz._

// WORKAROUND https://github.com/scalaz/scalaz/issues/1956
private[xmlformat] object FastTraverse {

  implicit final class IListExtras[A](private val self: IList[A])
      extends AnyVal {
    def traverseDisjunction[E, B](f: A => E \/ B): E \/ IList[B] = {
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
