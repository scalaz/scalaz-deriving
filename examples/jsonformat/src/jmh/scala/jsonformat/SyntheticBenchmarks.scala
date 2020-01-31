// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

// Copyright 2018 Andriy Plokhotnyuk

package jsonformat.benchmarks

import jsonformat._
import jsonformat.JsDecoder.ops._
import jsonformat.JsEncoder.ops._
import scalaz._, Scalaz._
import scalaz.annotation.deriving
import org.openjdk.jmh.annotations.{ Scope, Setup, State }
//import org.openjdk.jmh.annotations.Benchmark

// jsonformat/jmh:run -i 5 -wi 5 -f1 -t2 -w1 -r1 SyntheticBenchmarks.*
//
// see GoogleMapsAPIBenchmarks for profiling instructions

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
        o.n.fold(IList.empty[(String, JsValue)])(n =>
          IList.single("n" -> n.toJson)
        )
      )
    }
    import jsonformat.JsDecoder.fail
    implicit val decoder: JsDecoder[Nested] = {
      case JsObject(fields) =>
        if (fields.isEmpty) \/-(Nested(None))
        else
          fields.collectFirst {
            case ("n", v) => v.as[Nested]
          }.getOrElse(fail("n", JsObject(fields)))
      case other => fail("JsObject", other)
    }
  }
}

package s {
  @deriving(Equal, Show)
  final case class Nested(n: Option[Nested])
  object Nested {
    implicit val encoder: JsEncoder[Nested] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Nested] = DerivedProductJsDecoder.gen
  }
}
@State(Scope.Benchmark)
class SyntheticBenchmarks {
  //@Param(Array("100", "1000"))
  var size: Int                       = 500
  var objh: h.Nested                  = _
  var objm: m.Nested                  = _
  var objs: s.Nested                  = _
  var jsonString: String              = _
  var parsingErrorJsonString: String  = _
  var decodingErrorJsonString: String = _
  var ast1, ast2: JsValue             = _

  @Setup
  def setup(): Unit = {
    objh = 1.to(size).foldLeft(h.Nested(None))((n, _) => h.Nested(Some(n)))
    objm = 1.to(size).foldLeft(m.Nested(None))((n, _) => m.Nested(Some(n)))
    objs = 1.to(size).foldLeft(s.Nested(None))((n, _) => s.Nested(Some(n)))
    jsonString = CompactPrinter(encodeMagnolia())
    parsingErrorJsonString = jsonString.replace("{}", "xxx")
    decodingErrorJsonString = jsonString.replace("{}", "1")
    ast1 = JsParser(jsonString).getOrElse(null)
    ast2 = JsParser(decodingErrorJsonString).getOrElse(null)

    require(decodingErrorManual().isLeft)
    // require(parsingErrorManual().isLeft)
    require(decodeManual().getOrElse(null) == objh)
    require(CompactPrinter(encodeManual()) == jsonString)

    require(decodingErrorMagnolia().isLeft)
    // require(parsingErrorMagnolia().isLeft)
    require(decodeMagnoliaSuccess().getOrElse(null) == objm)
    require(CompactPrinter(encodeMagnolia()) == jsonString)

    require(decodingErrorShapeless().isLeft)
    // require(parsingErrorShapeless().isLeft)
    require(decodeShapelessSuccess().getOrElse(null) == objs)
    require(CompactPrinter(encodeShapeless()) == jsonString)
  }

  //@Benchmark
  def decodingErrorManual(): \/[String, h.Nested] =
    ast2.as[h.Nested]

  //@Benchmark
  def decodeManual(): \/[String, h.Nested] =
    ast1.as[h.Nested]

  //@Benchmark
  def encodeManual(): JsValue = objh.toJson

  //@Benchmark
  def decodingErrorMagnolia(): String \/ m.Nested =
    ast2.as[m.Nested]

  //@Benchmark
  def decodeMagnoliaSuccess(): String \/ m.Nested =
    ast1.as[m.Nested]

  //@Benchmark
  def encodeMagnolia(): JsValue = objm.toJson

  //@Benchmark
  def decodingErrorShapeless(): String \/ s.Nested =
    ast2.as[s.Nested]

  //@Benchmark
  def decodeShapelessSuccess(): String \/ s.Nested =
    ast1.as[s.Nested]

  //@Benchmark
  def encodeShapeless(): JsValue = objs.toJson

}
