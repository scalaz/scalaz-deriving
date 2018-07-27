// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration.Duration

import org.openjdk.jmh.annotations.{ State => Input, _ }

import scalaz._

import xmlformat.scalaxml._
import xmlformat.cord._
import xmlformat.stax._

// xmlformat/jmh:run -i 5 -wi 5 -f1 -t2 -w2 -r2 .*Benchmarks
//
// see org.openjdk.jmh.runner.options.CommandLineOptions
class Benchmarks {

  // the parser is more likely to run on multiple threads in the real scenario,
  // so running perf tests in this wrapper should help us stress the GC
  // behaviour more like reality.
  @inline final def parallel[A](f: =>A): Boolean = {
    Await.result(Future.sequence(List.fill(16)(Future(f))), Duration.Inf)
  }.nonEmpty

  @Benchmark
  def parseScalaXml(data: Data): Boolean = parallel {
    data.parseScala
  }

  @Benchmark
  def parseStax(data: Data): Boolean = parallel {
    data.parseStax
  }

  @Benchmark
  def printScalaXml(data: Data): Boolean = parallel {
    data.printScala
  }

  @Benchmark
  def printStax(data: Data): Boolean = parallel {
    data.printStax
  }

  @Benchmark
  def printCord(data: Data): Boolean = parallel {
    data.printCord
  }

  @Benchmark
  def printTree(data: Data): Boolean = parallel {
    data.printTree
  }

}

@Input(Scope.Benchmark)
class Data {
  val strings: List[String] = List(
    "scala-compiler-2.12.6.pom", // maven
    "Hannu_Rajaniemi", // wikipedia
    "numbering.xml" // docx content
  ).map(getResourceAsString(_))

  def parseScala = strings.map { s =>
    Decoder.parse(s) match {
      case \/-(XChildren(ICons(t, INil()))) => t
      case other                            => throw new IllegalArgumentException(other.toString)
    }
  }
  def parseStax = strings.map { s =>
    StaxDecoder.parse(s) match {
      case \/-(t) => t
      case other  => throw new IllegalArgumentException(other.toString)
    }
  }

  val parsed: List[XTag] = parseScala

  def printScala = parsed.map(t => Encoder.xnode.toScalaXml(t.asChild).toString)
  def printCord  = parsed.map(t => CordEncoder.encode(t))
  def printTree  = parsed.map(t => TreeEncoder.encode(t))
  def printStax  = parsed.map(t => StaxEncoder.encode(t))

  def getResourceAsString(res: String): String = {
    val is = getClass().getClassLoader().getResourceAsStream(res)
    try {
      val baos        = new java.io.ByteArrayOutputStream()
      val data        = Array.ofDim[Byte](2048)
      var len: Int    = 0
      def read(): Int = { len = is.read(data); len }
      while (read != -1) {
        baos.write(data, 0, len)
      }
      baos.toString("UTF-8")
    } finally {
      is.close()
    }
  }
}
