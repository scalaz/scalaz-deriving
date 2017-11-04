// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String
import scala.{ inline, Boolean, Int }

// a simple covariant typeclass
trait Default[A] {
  def default: A
}
object Default {
  @inline def apply[A](implicit i: Default[A]): Default[A] = i
  @inline def instance[A](a: => A): Default[A] = new Default[A] {
    override def default: A = a
  }

  implicit val int: Default[Int]         = instance(0)
  implicit val string: Default[String]   = instance("")
  implicit val boolean: Default[Boolean] = instance(false)

  implicit val derivation: TypeclassDerivation[Default] =
    new CovariantTypeclassDerivation[Default] {
      // performance optimisation
      override def map[A, B](fa: Default[A])(f: A => B): Default[B] =
        instance(f(fa.default))

      def point[A](a: => A): Default[A] = instance(a)

      def product2[A, B, C](fb: => Default[B], fc: => Default[C])(
        f: (B, C) => A
      ): Default[A] = instance(f(fb.default, fc.default))

      def coproduct2[A, B, C](fb: => Default[B], fc: => Default[C])(
        f: B \/ C => A
      ): Default[A] = instance(f(-\/(fb.default)))
    }

}
