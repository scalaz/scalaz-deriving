package scalaz

import java.lang.String

import scala.{ Boolean, Long }
import scala.Stream

import Scalaz._

import org.openjdk.jmh.annotations.{ State => Input, _ }

import com.danielasfregola.randomdatagenerator.RandomDataGenerator.random
import org.scalacheck.ScalacheckShapeless._

@deriving(Equal) sealed abstract class Fooz
@deriving(Equal) final case class Barz(s: String, i: Long) extends Fooz
@deriving(Equal) final case class Bazz(i: Long)            extends Fooz

sealed abstract class Foo
final case class Bar(s: String, i: Long) extends Foo
final case class Baz(i: Long)            extends Foo
object Foo {
  implicit val Equal: Equal[Foo] = { (a1, a2) =>
    (a1, a2) match {
      case (Bar(s1, i1), Bar(s2, i2)) => s1 === s2 && i1 === i2
      case (Baz(i1), Baz(i2))         => i1 === i2
      case _                          => false
    }
  }
}
@Input(Scope.Benchmark)
class Data {
  val zas = IList.fromList(Stream.continually(random[Fooz]).take(100).toList)
  val zbs = IList.fromList(Stream.continually(random[Fooz]).take(100).toList)

  val as = IList.fromList(Stream.continually(random[Foo]).take(100).toList)
  val bs = IList.fromList(Stream.continually(random[Foo]).take(100).toList)
}

// derivez/jmh:run -i 5 -wi 15 -f1 -t10 .*EqualBenchmark
class EqualBenchmark {

  @Benchmark
  def derivingEqual(data: Data): Boolean =
    data.zas === data.zbs

  @Benchmark
  def manualEqual(data: Data): Boolean =
    data.as === data.bs

}
