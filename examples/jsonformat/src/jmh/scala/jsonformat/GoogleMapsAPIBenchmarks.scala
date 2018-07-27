// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

// Copyright 2018 Andriy Plokhotnyuk

package jsonformat.benchmarks

import java.util.concurrent.TimeUnit

import org.openjdk.jmh
import io.circe.{ Error, Printer }
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import jsonformat._
import jsonformat.JsDecoder.ops._
import jsonformat.JsEncoder.ops._
import scalaz._
import Scalaz._
import jsonformat.BenchmarkUtils.getResourceAsString

import scala.collection.immutable.IndexedSeq

// sbt clean 'jsonformat/jmh:run GoogleMapsAPIBenchmarks.*'
// or
// sbt clean 'jsonformat/jmh:run -jvm /usr/lib/jvm/graalvm-ee-1.0.0-rc3/bin/java -wi 10 GoogleMapsAPIBenchmarks.*'
//
// see org.openjdk.jmh.runner.options.CommandLineOptions

@deriving(JsEncoder, JsDecoder)
case class Value(text: String, value: Int)

@deriving(JsEncoder, JsDecoder)
case class Elements(distance: Value, duration: Value, status: String)

@deriving(JsEncoder, JsDecoder)
case class Rows(elements: IndexedSeq[Elements])

@deriving(JsEncoder, JsDecoder)
case class DistanceMatrix(
  destination_addresses: IndexedSeq[String],
  origin_addresses: IndexedSeq[String],
  rows: IndexedSeq[Rows],
  status: String
)

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
  val circePrinter: Printer =
    Printer.noSpaces.copy(dropNullValues = true, reuseWriters = true)
  var jsonString: String  = _
  var jsonString2: String = _
  var obj: DistanceMatrix = _

  @jmh.annotations.Setup
  def setup(): Unit = {
    //Distance Matrix API call for top-10 by population cities in US:
    //https://maps.googleapis.com/maps/api/distancematrix/json?origins=New+York|Los+Angeles|Chicago|Houston|Phoenix+AZ|Philadelphia|San+Antonio|San+Diego|Dallas|San+Jose&destinations=New+York|Los+Angeles|Chicago|Houston|Phoenix+AZ|Philadelphia|San+Antonio|San+Diego|Dallas|San+Jose
    jsonString = getResourceAsString("google_maps_api_response.json")
    jsonString2 = getResourceAsString("google_maps_api_compact_response.json")
    obj = readCirce().right.get
    require(readCirce().right.get == obj)
    require(writeCirce() == jsonString2)
    require(readScalazDeriving().getOrElse(null) == obj)
    require(writeScalazDeriving() == jsonString2)
  }

  @jmh.annotations.Benchmark
  def readCirce(): Either[Error, DistanceMatrix] =
    decode[DistanceMatrix](jsonString)

  @jmh.annotations.Benchmark
  def readScalazDeriving(): \/[String, DistanceMatrix] =
    JsParser(jsonString).flatMap(_.as[DistanceMatrix])

  @jmh.annotations.Benchmark
  def writeCirce(): String = circePrinter.pretty(obj.asJson)

  @jmh.annotations.Benchmark
  def writeScalazDeriving(): String = CompactPrinter(obj.toJson)
}
