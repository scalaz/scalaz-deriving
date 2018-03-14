/*
rule = "class:fix.Deriving_0_11_0"
*/
package scalaz {
  object Derivez {
    def gen[F[_], A]: F[A] = ???
  }
}

package fix {
  trait Tc1[A] {
    def xmap[A, B](f: A => B, g: B => A): Tc1[B] = null
  }
  object Tc1 {
    implicit val string: Tc1[String] = null
  }
}

