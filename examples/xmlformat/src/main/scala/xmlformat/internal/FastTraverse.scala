// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat.internal

import scalaz._

// WORKAROUND https://github.com/scalaz/scalaz/issues/1956
private[xmlformat] object FastTraverse {

  implicit final class IListExtras[A](private val self: IList[A])
      extends AnyVal {
    def traverseDisjunction[E, B](f: A => E \/ B): E \/ IList[B] =
      try {
        val res = self.map { a =>
          f(a) match {
            case -\/(err) => throw new EarlyExit(err) // scalafix:ok
            case \/-(j)   => j
          }
        }
        \/-(res)
      } catch {
        case EarlyExit(msg) => -\/(msg.asInstanceOf[E])
      }
  }
}

private[internal] final case class EarlyExit[E](msg: E)
    extends Exception
    with util.control.NoStackTrace
