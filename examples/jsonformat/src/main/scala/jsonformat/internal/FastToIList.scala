// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat.internal

import scalaz._

private[jsonformat] object FastToIList {

  implicit final class SeqExtras[A](private val self: Seq[A]) extends AnyVal {
    // there is no Foldable[Seq] so this is a suitable replacement
    def toIList: IList[A]                  = self.foldRight(IList.empty[A])(_ :: _)
    def mapToIList[B](f: A => B): IList[B] =
      self.foldRight(IList.empty[B])(f(_) :: _)
  }

  implicit final class MapExtras[A, B](private val self: Map[A, B])
      extends AnyVal {
    // name clash with the Foldable syntax
    def asIList: IList[(A, B)] = self.foldRight(IList.empty[(A, B)])(_ :: _)
    //def mapToIList[B](f: A => B): IList[B] = self.foldRight(IList.empty[B])(f(_) :: _)
  }

}
