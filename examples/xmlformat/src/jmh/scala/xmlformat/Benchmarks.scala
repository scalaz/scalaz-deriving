// Copyright: 2017 - 2021 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat

import org.openjdk.jmh.annotations.{ State => Input, _ }

import scalaz._

import xmlformat.stax._

// xmlformat/jmh:run -i 5 -wi 5 -f1 -t1 -w1 -r1 .*Benchmarks
//
// see org.openjdk.jmh.runner.options.CommandLineOptions
class Benchmarks {

  @Benchmark
  def parseStax(data: Data) = data.parseStax

  @Benchmark
  def printStax(data: Data) = data.printStax

}

@Input(Scope.Benchmark)
class Data {
  val strings: List[String] = List(
    "scala-compiler-2.12.6.pom", // maven
    "Hannu_Rajaniemi",           // wikipedia
    "numbering.xml"              // docx content
  ).map(getResourceAsString(_))

  def parseStax =
    strings.map { s =>
      StaxDecoder.parse(s) match {
        case \/-(t) => t
        case other  => throw new IllegalArgumentException(other.toString)
      }
    }

  val parsed: List[XTag] = parseStax

  def printStax = parsed.map(t => StaxEncoder.encode(t))

  def getResourceAsString(res: String): String = {
    val is = getClass().getClassLoader().getResourceAsStream(res)
    try {
      val baos     = new java.io.ByteArrayOutputStream()
      val data     = Array.ofDim[Byte](2048)
      var len: Int = 0
      def read(): Int = { len = is.read(data); len }
      while (read != -1)
        baos.write(data, 0, len)
      baos.toString("UTF-8")
    } finally is.close()
  }
}
