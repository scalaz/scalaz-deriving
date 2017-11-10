// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ Int }

/**
 * Represents a value and a type constructor that can operate on that value.
 *
 * The type of value is hidden, but since the types align we can apply value to
 * tc.
 */
sealed abstract class ParamX[F[_]] {
  type T
  def value: T
  def tc: F[T]

  // TODO: def label: String
  // However, this is not trivial from a lawfullnes point of view.
  // The existing methods can use tuple-style labels, but it will
  // lead to laws being broken, for example:
  // divide2(divide2(a, b), divide2(c, d)) != divide4(a, b, c, d)
}
object ParamX {
  type Aux[F[_], A] = ParamX[F] { type T = A }

  def apply[A, F[_]](a: A, fa: => F[A]): ParamX.Aux[F, A] =
    new ParamX[F] {
      type T = A
      def value: A = a
      def tc: F[A] = fa
    }
}

/**
 * Represents a Coproduct of unknown arity, of type constructers that have a
 * type member.
 */
sealed abstract class CoproductX[F[_]] {
  private[scalaz] def index: Int
  def get: ParamX[F]
}
object CoproductX {
  def apply[F[_]](i: Int, el: ParamX[F]): CoproductX[F] =
    new CoproductX[F] {
      def index: Int     = i
      def get: ParamX[F] = el
    }

  /**
   * Logical AND of CoproductX instances that have been generated from the same
   * function. Returns a Just if both instances represent the same element in
   * the coproduct space, otherwise Empty.
   *
   * This is not strictly Total because the scala type system is not capable of
   * expressing the type we are returning in the functions (without knowing the
   * length of the list at compiletime, which is a fundamentally different
   * approach).
   *
   * These methods are safe so long as the *range* of the function is the same
   * (i.e. the arity and refined types of all the contents are the same), but
   * will break if the range varies. A malicious (or incompetent) developer
   * could inject such a function, but in practice the functions are generated
   * by macros and are highly regular and meet the formal requirement.
   *
   * The function is provided as a guard to avoid (the very unsound!)
   * combination of raw CoproductX instances.
   */
  def and[Z, F[_]](f: Z => CoproductX[F])(z1: Z, z2: Z): Maybe[Two[F]] = {
    import Scalaz._

    val c1 = f(z1)
    val c2 = f(z2)

    if (c1.index == c2.index)
      (c1.get, c2.get).just.asInstanceOf[Maybe[Two[F]]]
    else
      Maybe.empty
  }

  // TODO: return something that doesn't duplicate the .tc field
  type Two[F[_]] =
    (ParamX.Aux[F, A], ParamX.Aux[F, A]) forSome { type A }

}

sealed abstract class ProductX[F[_]] {
  def get: IList[ParamX[F]]
}
object ProductX {
  def apply[F[_]](els: IList[ParamX[F]]): ProductX[F] = new ProductX[F] {
    def get: IList[ParamX[F]] = els
  }

  // same deal as CoproductX.and
  def and[Z, F[_]](f: Z => ProductX[F])(z1: Z, z2: Z): Two[F] = {
    val p1 = f(z1)
    val p2 = f(z2)
    // assume zip works by the Range argument
    (p1.get zip p2.get).asInstanceOf[Two[F]]
  }

  // TODO: return something that doesn't duplicate the .tc field
  type Two[F[_]] =
    IList[(ParamX.Aux[F, A], ParamX.Aux[F, A]) forSome { type A }]

}
