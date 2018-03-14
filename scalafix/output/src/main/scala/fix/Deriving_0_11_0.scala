package fix

import scalaz.deriving
import scalaz.xderiving

object Deriving_0_11_0_Test {

  @xderiving(Tc1) final case class Foo1(value: String) extends scala.AnyVal

  @xderiving(Tc1) final class Foo2(val value: String) extends AnyVal

  @deriving(Tc1)
  final case class Foo3(value: String)

  @deriving(Tc1)
  final class Foo4(value: String)

}
