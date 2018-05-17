// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scalaz._, Scalaz._

package cord {
  final case class Cords(val cord: Cord) extends AnyVal
  object Cords {
    // scalafix:off DisableSyntax.implicitConversion
    implicit def trivial(c: Cord): Cords   = Cords(c)
    implicit def mat[A: Show](a: A): Cords = Cords(Show[A].show(a))
    // scalafix:on DisableSyntax.implicitConversion
  }
}

package object cord {
  import StringContext.{ treatEscapes => esc }
  import Cord.{ stringToCord => toCord }

  implicit class CordInterpolator(private val sc: StringContext)
      extends AnyVal {
    def cord(args: Cords*): Cord = {
      val strings = sc.parts.toList.toIList.map(s => toCord(esc(s)))
      val cords   = args.toList.toIList.map(_.cord)
      strings.interleave(cords).fold
    }
  }
}
