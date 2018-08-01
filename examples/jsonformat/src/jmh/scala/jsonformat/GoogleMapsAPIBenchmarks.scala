// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

// Copyright 2018 Andriy Plokhotnyuk

package jsonformat.benchmarks

import java.util.concurrent.TimeUnit

import org.openjdk.jmh
import jsonformat._
import jsonformat.JsDecoder.ops._
import jsonformat.JsEncoder.ops._
import scalaz._
import Scalaz._
import jsonformat.BenchmarkUtils.getResourceAsString

// sbt clean 'jsonformat/jmh:run GoogleMapsAPIBenchmarks.*'
// or
// sbt clean 'jsonformat/jmh:run -jvm /usr/lib/jvm/graalvm-ee-1.0.0-rc3/bin/java -wi 10 GoogleMapsAPIBenchmarks.*'
//
// see org.openjdk.jmh.runner.options.CommandLineOptions

package m {
  @deriving(JsEncoder, JsDecoder)
  final case class Value(text: String, value: Int)

  @deriving(JsEncoder, JsDecoder)
  final case class Elements(distance: Value, duration: Value, status: String)

  @deriving(JsEncoder, JsDecoder)
  final case class Rows(elements: IList[Elements])

  @deriving(JsEncoder, JsDecoder)
  final case class DistanceMatrix(
    destination_addresses: IList[String],
    origin_addresses: IList[String],
    rows: IList[Rows],
    status: String
  )
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
class GoogleMapsAPIBenchmarks {
  var jsonString: String     = _
  var jsonString2: String    = _
  var jsonString3: String    = _
  var objm: m.DistanceMatrix = _
  var ast1, ast2: JsValue    = _

  @jmh.annotations.Setup
  def setup(): Unit = {
    //Distance Matrix API call for top-10 by population cities in US:
    //https://maps.googleapis.com/maps/api/distancematrix/json?origins=New+York|Los+Angeles|Chicago|Houston|Phoenix+AZ|Philadelphia|San+Antonio|San+Diego|Dallas|San+Jose&destinations=New+York|Los+Angeles|Chicago|Houston|Phoenix+AZ|Philadelphia|San+Antonio|San+Diego|Dallas|San+Jose
    jsonString = getResourceAsString("google_maps_api_response.json")
    jsonString2 = getResourceAsString("google_maps_api_compact_response.json")
    jsonString3 = getResourceAsString("google_maps_api_error_response.json")
    ast1 = JsParser(jsonString).getOrElse(null)
    ast2 = JsParser(jsonString3).getOrElse(null)

    objm = decodeMagnolia().getOrElse(null)
    require(CompactPrinter(encodeMagnolia()) == jsonString2)

    require(decodeMagnoliaError().isLeft)
  }

  @jmh.annotations.Benchmark
  def decodeMagnolia(): \/[String, m.DistanceMatrix] =
    ast1.as[m.DistanceMatrix]

  @jmh.annotations.Benchmark
  def decodeMagnoliaError(): \/[String, m.DistanceMatrix] =
    ast2.as[m.DistanceMatrix]

  @jmh.annotations.Benchmark
  def encodeMagnolia(): JsValue = objm.toJson
}
