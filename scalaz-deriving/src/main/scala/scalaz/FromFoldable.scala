// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

import Scalaz._

/** Better than CanBuildFrom but still hacky as hell */
trait FromFoldable[F[_]] {
  def fromFoldable[G[_]: Foldable1, A](g: G[A]): F[A]
}
object FromFoldable {
  @inline def apply[F[_]](implicit i: FromFoldable[F]): FromFoldable[F] = i
}

trait FromFoldable1[F[_]] {
  def fromFoldable1[G[_]: Foldable1, A](g: G[A]): F[A]
}
object FromFoldable1 {
  @inline def apply[F[_]](implicit i: FromFoldable1[F]): FromFoldable1[F] = i

  // lossy implementation
  implicit val maybe: FromFoldable1[Maybe] = new FromFoldable1[Maybe] {
    override def fromFoldable1[G[_]: Foldable1, A](g: G[A]): Maybe[A] =
      g.index(0).toMaybe
  }

  implicit val estream: FromFoldable1[EphemeralStream] =
    new FromFoldable1[EphemeralStream] {
      override def fromFoldable1[G[_]: Foldable1, A](
        g: G[A]
      ): EphemeralStream[A] =
        g.toEphemeralStream
    }

  implicit val nel: FromFoldable1[NonEmptyList] =
    new FromFoldable1[NonEmptyList] {
      override def fromFoldable1[G[_]: Foldable1, A](
        g: G[A]
      ): NonEmptyList[A] =
        g.toNel
    }

  // lossy
  implicit val id: FromFoldable1[Id] =
    new FromFoldable1[Id] {
      override def fromFoldable1[G[_]: Foldable1, A](
        g: G[A]
      ): Id[A] =
        g.toNel.head
    }
}
