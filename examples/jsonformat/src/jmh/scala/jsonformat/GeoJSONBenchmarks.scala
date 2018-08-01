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

// sbt clean 'jsonformat/jmh:run GeoJSONBenchmarks.*'
// or
// sbt clean 'jsonformat/jmh:run -jvm /usr/lib/jvm/graalvm-ee-1.0.0-rc3/bin/java -wi 10 GeoJSONBenchmarks.*'
//
// see org.openjdk.jmh.runner.options.CommandLineOptions

package m {

  @deriving(JsEncoder, JsDecoder)
  sealed abstract class Geometry
  object Geometry {
    implicit val tupleDblDblEncoder: JsEncoder[(Double, Double)] = {
      case (lat, lon) => JsArray(IList(lat.toJson, lon.toJson))
    }
    implicit val tupleDblDblDecoder: JsDecoder[(Double, Double)] =
      JsDecoder[IList[Double]].emap {
        case ICons(car, ICons(ccar, INil())) => \/-((car, ccar))
        case other =>
          JsDecoder.fail("2 numbers", JsArray(other.map(JsDouble(_))))
      }
  }

  final case class Point(coordinates: (Double, Double)) extends Geometry
  final case class MultiPoint(coordinates: IList[(Double, Double)])
      extends Geometry
  final case class LineString(coordinates: IList[(Double, Double)])
      extends Geometry
  final case class MultiLineString(
    coordinates: IList[IList[(Double, Double)]]
  ) extends Geometry
  final case class Polygon(coordinates: IList[IList[(Double, Double)]])
      extends Geometry
  final case class MultiPolygon(
    coordinates: IList[IList[IList[(Double, Double)]]]
  ) extends Geometry
  final case class GeometryCollection(geometries: IList[Geometry])
      extends Geometry

  @deriving(JsEncoder, JsDecoder)
  sealed abstract class GeoJSON
  final case class Feature(properties: Map[String, String], geometry: Geometry)
      extends GeoJSON
  final case class FeatureCollection(features: IList[GeoJSON]) extends GeoJSON
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
class GeoJSONBenchmarks {
  var jsonString: String    = _
  var jsonString2: String   = _
  var jsonStringErr: String = _
  var objm: m.GeoJSON       = _
  var ast1, ast2: JsValue   = _

  @jmh.annotations.Setup
  def setup(): Unit = {
    jsonString = getResourceAsString("che.geo.json")
    jsonString2 = getResourceAsString("che-2.geo.json")
    jsonStringErr = getResourceAsString("che-err.geo.json")
    ast1 = JsParser(jsonString).getOrElse(null)
    ast2 = JsParser(jsonStringErr).getOrElse(null)
    objm = decodeMagnolia().getOrElse(null)
    require(decodeMagnoliaError.isLeft)
    require(CompactPrinter(encodeMagnolia()) == jsonString)
  }

  @jmh.annotations.Benchmark
  def decodeMagnolia(): \/[String, m.GeoJSON] =
    ast1.as[m.GeoJSON]

  @jmh.annotations.Benchmark
  def decodeMagnoliaError(): String \/ m.GeoJSON =
    ast2.as[m.GeoJSON]

  // https://github.com/propensive/magnolia/issues/117
  @jmh.annotations.Benchmark
  def encodeMagnolia(): JsValue = objm.toJson

}
