// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline, Unit }

// will hopefully replace Divisible in scalaz7.3
trait LazyDivisible[F[_]] extends LazyDivide[F] with ApplicativeDivisible[F] {
  // universally quantified instance of F[_]
  def conquer[A]: F[A]

  override def contramap[A, B](fa: F[A])(f: B => A): F[B] =
    divide2(conquer[Unit], fa)(c => ((), f(c)))

  // ApplicativeDivisible impl
  override final def xproduct0[Z](z: => Z): F[Z] = conquer
}
object LazyDivisible {
  @inline def apply[F[_]](implicit i: LazyDivisible[F]): LazyDivisible[F] = i

  import java.lang.String
  import scala.StringContext

  sealed trait L[F[_], A]
  final case class Label[F[_], A](fa: F[A], label: String) extends L[F, A]
  final case class Solo[F[_], A](fa: F[A])                 extends L[F, A]

  implicit val tcdShow: LazyDivisible[L[Show, ?]] =
    new LazyDivisible[L[Show, ?]] {
      def conquer[A]: L[Show, A] = Solo(Show.shows(_ => ""))
      def divide2[A, B, C](fa: => L[Show, A], fb: => L[Show, B])(
        f: C => (A, B)
      ): L[Show, C] = Solo(
        Show.shows { c =>
          val (a, b) = f(c)
          (fa, fb) match {
            case (Label(fa, la), Label(fb, lb)) =>
              s"$la=${fa.shows(a)},$lb=${fb.shows(b)}"
            case (Label(fa, la), Solo(fb)) =>
              s"$la=${fa.shows(a)},${fb.shows(b)}"
            case (Solo(fa), Label(fb, lb)) =>
              s"${fa.shows(a)},$lb=${fb.shows(b)}"
            case (Solo(fa), Solo(fb)) =>
              s"${fa.shows(a)},${fb.shows(b)}"
          }
        }
      )
    }
}
