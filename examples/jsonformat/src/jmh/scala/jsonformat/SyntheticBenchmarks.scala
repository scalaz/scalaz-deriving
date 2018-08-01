// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

// Copyright 2018 Andriy Plokhotnyuk

package jsonformat.benchmarks

import java.util.concurrent.TimeUnit

import org.openjdk.jmh
import jsonformat._
import jsonformat.JsDecoder.ops._
import jsonformat.JsEncoder.ops._
import scalaz._, Scalaz._

// sbt clean 'jsonformat/jmh:run SyntheticBenchmarks.*'
// or
// sbt clean 'jsonformat/jmh:run -jvm /usr/lib/jvm/graalvm-ee-1.0.0-rc3/bin/java -wi 10 SyntheticBenchmarks.*'
//
// see org.openjdk.jmh.runner.options.CommandLineOptions

// magnolia
package m {
  @deriving(Equal, Show, JsEncoder, JsDecoder)
  final case class Nested(n: Option[Nested])
}

// hand-rolled
package h {
  @deriving(Equal, Show)
  final case class Nested(n: Option[Nested])
  object Nested {
    implicit val encoder: JsEncoder[Nested] = { o =>
      JsObject(
        o.n.fold(IList.empty[(String, JsValue)])(
          n => IList.single("n" -> n.toJson)
        )
      )
    }
    implicit val decoder: JsDecoder[Nested] = { j =>
      j.asJsObject.flatMap { obj =>
        if (obj.fields.isEmpty) \/-(Nested(None))
        else obj.getAs[Option[Nested]]("n").map(Nested(_))
      }
    }
  }
}

@jmh.annotations.State(jmh.annotations.Scope.Thread)
@jmh.annotations.Warmup(iterations = 7, time = 1, timeUnit = TimeUnit.SECONDS)
@jmh.annotations.Measurement(
  iterations = 7,
  time = 1,
  timeUnit = TimeUnit.SECONDS
)
@jmh.annotations.Fork(
  value = 1,
  jvmArgs = Array(
    "-Xss2m",
    "-Xms2g",
    "-Xmx2g",
    "-XX:+UseG1GC",
    "-XX:+AlwaysPreTouch"
  )
)
@jmh.annotations.BenchmarkMode(Array(jmh.annotations.Mode.Throughput))
@jmh.annotations.OutputTimeUnit(TimeUnit.SECONDS)
class SyntheticBenchmarks {
  @jmh.annotations.Param(Array("10", "100"))
  var size: Int                       = 7
  var obj2: h.Nested                  = _
  var obj3: m.Nested                  = _
  var jsonString: String              = _
  var parsingErrorJsonString: String  = _
  var decodingErrorJsonString: String = _
  var ast1, ast2: JsValue             = _

  @jmh.annotations.Setup
  def setup(): Unit = {
    obj2 = 1.to(size).foldLeft(h.Nested(None))((n, _) => h.Nested(Some(n)))
    obj3 = 1.to(size).foldLeft(m.Nested(None))((n, _) => m.Nested(Some(n)))
    jsonString = CompactPrinter(encodeMagnolia())
    parsingErrorJsonString = jsonString.replace("{}", "xxx")
    decodingErrorJsonString = jsonString.replace("{}", "1")
    ast1 = JsParser(jsonString).getOrElse(null)
    ast2 = JsParser(decodingErrorJsonString).getOrElse(null)

    require(decodingErrorManual().isLeft)
    require(decodingErrorMagnolia().isLeft)
    // require(parsingErrorManual().isLeft)
    // require(parsingErrorMagnolia().isLeft)
    require(decodeManual().getOrElse(null) == obj2)
    require(decodeMagnolia().getOrElse(null) == obj3)
    require(CompactPrinter(encodeManual()) == jsonString)
    require(CompactPrinter(encodeMagnolia()) == jsonString)
  }

  @jmh.annotations.Benchmark
  def decodingErrorManual(): \/[String, h.Nested] =
    ast2.as[h.Nested]

  @jmh.annotations.Benchmark
  def decodingErrorMagnolia(): String \/ m.Nested =
    ast2.as[m.Nested]

  @jmh.annotations.Benchmark
  def decodeManual(): \/[String, h.Nested] =
    ast1.as[h.Nested]

  @jmh.annotations.Benchmark
  def decodeMagnolia(): String \/ m.Nested =
    ast1.as[m.Nested]

  @jmh.annotations.Benchmark
  def encodeManual(): JsValue = obj2.toJson

  @jmh.annotations.Benchmark
  def encodeMagnolia(): JsValue = obj3.toJson

}
