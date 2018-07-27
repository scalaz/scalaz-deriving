// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

// Copyright 2018 Andriy Plokhotnyuk

package jsonformat.benchmarks

import java.util.concurrent.TimeUnit

import org.openjdk.jmh
import io.circe.{ Decoder, Encoder, Error, Printer }
import io.circe.parser._
import io.circe.syntax._
import io.circe._
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._
import jsonformat._
import jsonformat.JsDecoder.ops._
import jsonformat.JsEncoder.ops._
import scalaz._
import Scalaz._
import jsonformat.BenchmarkUtils.getResourceAsString

import scala.collection.immutable.IndexedSeq

// sbt clean 'jsonformat/jmh:run GeoJSONBenchmarks.*'
// or
// sbt clean 'jsonformat/jmh:run -jvm /usr/lib/jvm/graalvm-ee-1.0.0-rc3/bin/java -wi 10 GeoJSONBenchmarks.*'
//
// see org.openjdk.jmh.runner.options.CommandLineOptions

@deriving(JsEncoder, JsDecoder)
sealed abstract class Geometry
object Geometry {
  implicit val tupleDblDblEncoder: JsEncoder[(Double, Double)] = {
    case (lat, lon) => JsArray(IList(lat.toJson, lon.toJson))
  }
  implicit val tupleDblDblDecoder: JsDecoder[(Double, Double)] = { j =>
    j.as[IndexedSeq[Double]].flatMap { obj =>
      if (obj.size == 2) \/-((obj(0), obj(1)))
      else -\/("")
    }
  }
  implicit val config: Configuration =
    Configuration.default.withDiscriminator("type")
  implicit val pointEncoder: Encoder[Point]                     = deriveEncoder
  implicit val pointDecoder: Decoder[Point]                     = deriveDecoder
  implicit val multiPointEncoder: Encoder[MultiPoint]           = deriveEncoder
  implicit val multiPointDecoder: Decoder[MultiPoint]           = deriveDecoder
  implicit val lineStringEncoder: Encoder[LineString]           = deriveEncoder
  implicit val lineStringDecoder: Decoder[LineString]           = deriveDecoder
  implicit val multiLineStringEncoder: Encoder[MultiLineString] = deriveEncoder
  implicit val multiLineStringDecoder: Decoder[MultiLineString] = deriveDecoder
  implicit val polygonEncoder: Encoder[Polygon]                 = deriveEncoder
  implicit val polygonDecoder: Decoder[Polygon]                 = deriveDecoder
  implicit val multiPolygonEncoder: Encoder[MultiPolygon]       = deriveEncoder
  implicit val multiPolygonDecoder: Decoder[MultiPolygon]       = deriveDecoder
  implicit val geometryCollectionEncoder: Encoder[GeometryCollection] =
    deriveEncoder
  implicit val geometryCollectionDecoder: Decoder[GeometryCollection] =
    deriveDecoder
  implicit val geometryEncoder: Encoder[Geometry] = deriveEncoder
  implicit val geometryDecoder: Decoder[Geometry] = deriveDecoder
}
case class Point(coordinates: (Double, Double)) extends Geometry
case class MultiPoint(coordinates: IndexedSeq[(Double, Double)])
    extends Geometry
case class LineString(coordinates: IndexedSeq[(Double, Double)])
    extends Geometry
case class MultiLineString(
  coordinates: IndexedSeq[IndexedSeq[(Double, Double)]]
) extends Geometry
case class Polygon(coordinates: IndexedSeq[IndexedSeq[(Double, Double)]])
    extends Geometry
case class MultiPolygon(
  coordinates: IndexedSeq[IndexedSeq[IndexedSeq[(Double, Double)]]]
) extends Geometry
case class GeometryCollection(geometries: IndexedSeq[Geometry]) extends Geometry

@deriving(JsEncoder, JsDecoder)
sealed abstract class GeoJSON
object GeoJSON {
  implicit val config: Configuration =
    Configuration.default.withDiscriminator("type")
  implicit val featureEncoder: Encoder[Feature] = deriveEncoder
  implicit val featureDecoder: Decoder[Feature] = deriveDecoder
  implicit val featureCollectionEncoder: Encoder[FeatureCollection] =
    deriveEncoder
  implicit val featureCollectionDecoder: Decoder[FeatureCollection] =
    deriveDecoder
  implicit val geoJSONEncoder: Encoder[GeoJSON] = deriveEncoder
  implicit val geoJSONDecoder: Decoder[GeoJSON] = deriveDecoder
}
case class Feature(properties: Map[String, String], geometry: Geometry)
    extends GeoJSON
case class FeatureCollection(features: IndexedSeq[GeoJSON]) extends GeoJSON

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
  val circePrinter: Printer =
    Printer.noSpaces.copy(dropNullValues = true, reuseWriters = true)
  var jsonString: String  = _
  var jsonString2: String = _
  var obj: GeoJSON        = _

  @jmh.annotations.Setup
  def setup(): Unit = {
    jsonString = getResourceAsString("che.geo.json")
    jsonString2 = getResourceAsString("che-2.geo.json")
    obj = readCirce().right.get
    require(readCirce().right.get == obj)
    require(writeCirce() == jsonString2)
    require(readScalazDeriving().getOrElse(null) == obj)
    require(writeScalazDeriving() == jsonString)
  }

  @jmh.annotations.Benchmark
  def readCirce(): Either[Error, GeoJSON] = decode[GeoJSON](jsonString)

  @jmh.annotations.Benchmark
  def readScalazDeriving(): \/[String, GeoJSON] =
    JsParser(jsonString).flatMap(_.as[GeoJSON])

  @jmh.annotations.Benchmark
  def writeCirce(): String = circePrinter.pretty(obj.asJson)

  @jmh.annotations.Benchmark
  def writeScalazDeriving(): String = CompactPrinter(obj.toJson)
}
