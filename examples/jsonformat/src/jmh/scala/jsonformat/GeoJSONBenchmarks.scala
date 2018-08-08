// Copyright: 2010 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

// Copyright 2018 Andriy Plokhotnyuk

package jsonformat.benchmarks

import jsonformat._
import jsonformat.JsDecoder.ops._
import jsonformat.JsEncoder.ops._
import scalaz._, Scalaz._
import jsonformat.BenchmarkUtils.getResourceAsString
import org.openjdk.jmh.annotations.{ Benchmark, Scope, Setup, State }

// jsonformat/jmh:run -i 5 -wi 5 -f1 -t2 -w1 -r1 GeoJSONBenchmarks.*
//
// see GoogleMapsAPIBenchmarks for profiling instructions

object orphans {
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
import orphans._

package m {

  @deriving(JsEncoder, JsDecoder)
  sealed abstract class Geometry
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

package s {

  sealed abstract class Geometry
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

  sealed abstract class GeoJSON
  final case class Feature(properties: Map[String, String], geometry: Geometry)
      extends GeoJSON
  final case class FeatureCollection(features: IList[GeoJSON]) extends GeoJSON

  object Geometry {
    implicit val encoder: JsEncoder[Geometry] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Geometry] = DerivedCoproductJsDecoder.gen
  }
  object Point {
    implicit val encoder: JsEncoder[Point] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Point] = DerivedProductJsDecoder.gen
  }
  object MultiPoint {
    implicit val encoder: JsEncoder[MultiPoint] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[MultiPoint] = DerivedProductJsDecoder.gen
  }
  object LineString {
    implicit val encoder: JsEncoder[LineString] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[LineString] = DerivedProductJsDecoder.gen
  }
  object MultiLineString {
    implicit val encoder: JsEncoder[MultiLineString] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[MultiLineString] =
      DerivedProductJsDecoder.gen
  }
  object Polygon {
    implicit val encoder: JsEncoder[Polygon] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Polygon] = DerivedProductJsDecoder.gen
  }
  object MultiPolygon {
    implicit val encoder: JsEncoder[MultiPolygon] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[MultiPolygon] = DerivedProductJsDecoder.gen
  }
  object GeometryCollection {
    implicit val encoder: JsEncoder[GeometryCollection] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[GeometryCollection] =
      DerivedProductJsDecoder.gen
  }

  object GeoJSON {
    implicit val encoder: JsEncoder[GeoJSON] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[GeoJSON] = DerivedCoproductJsDecoder.gen
  }
  object Feature {
    implicit val encoder: JsEncoder[Feature] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Feature] = DerivedProductJsDecoder.gen
  }
  object FeatureCollection {
    implicit val encoder: JsEncoder[FeatureCollection] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[FeatureCollection] =
      DerivedProductJsDecoder.gen
  }

}

@State(Scope.Benchmark)
class GeoJSONBenchmarks {
  var jsonString: String    = _
  var jsonString2: String   = _
  var jsonStringErr: String = _
  var objm: m.GeoJSON       = _
  var objs: s.GeoJSON       = _
  var ast1, ast2: JsValue   = _

  @Setup
  def setup(): Unit = {
    jsonString = getResourceAsString("che.geo.json")
    jsonString2 = getResourceAsString("che-2.geo.json")
    jsonStringErr = getResourceAsString("che-err.geo.json")
    ast1 = JsParser(jsonString).getOrElse(null)
    ast2 = JsParser(jsonStringErr).getOrElse(null)
    objm = decodeMagnolia().getOrElse(null)
    require(decodeMagnoliaError.isLeft)
    require(CompactPrinter(encodeMagnolia()) == jsonString)
    objs = decodeShapeless().getOrElse(null)
    require(decodeShapelessError.isLeft)
    require(CompactPrinter(encodeShapeless()) == jsonString)
  }

  @Benchmark
  def decodeMagnolia(): \/[String, m.GeoJSON] =
    ast1.as[m.GeoJSON]

  @Benchmark
  def decodeMagnoliaError(): String \/ m.GeoJSON =
    ast2.as[m.GeoJSON]

  @Benchmark
  def encodeMagnolia(): JsValue = objm.toJson

  @Benchmark
  def decodeShapeless(): \/[String, s.GeoJSON] =
    ast1.as[s.GeoJSON]

  @Benchmark
  def decodeShapelessError(): String \/ s.GeoJSON =
    ast2.as[s.GeoJSON]

  @Benchmark
  def encodeShapeless(): JsValue = objs.toJson

}
