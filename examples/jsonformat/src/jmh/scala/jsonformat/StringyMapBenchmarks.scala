// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat.benchmarks

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

import jsonformat._
import org.openjdk.jmh.annotations.{ Benchmark, Scope, Setup, State }
import scalaz._, Scalaz._

// jsonformat/jmh:run -i 5 -wi 5 -f1 -t2 -w1 -r1 StringyMapBenchmarks.*

@State(Scope.Benchmark)
class StringyMapBenchmarks {
  val keys_lots_interned: IList[String] = IList(
    "id",
    "id_str",
    "name",
    "screen_name",
    "location",
    "description",
    "url",
    "entities",
    "protected",
    "followers_count",
    "friends_count",
    "listed_count",
    "created_at",
    "favourites_count",
    "utc_offset",
    "time_zone",
    "geo_enabled",
    "verified",
    "statuses_count",
    "lang",
    "contributors_enabled",
    "is_translator",
    "is_translation_enabled",
    "profile_background_color",
    "profile_background_image_url",
    "profile_background_image_url_https",
    "profile_background_tile",
    "profile_image_url",
    "profile_image_url_https",
    "profile_banner_url",
    "profile_link_color",
    "profile_sidebar_border_color",
    "profile_sidebar_fill_color",
    "profile_text_color",
    "profile_use_background_image",
    "has_extended_profile",
    "default_profile",
    "default_profile_image",
    "following",
    "follow_request_sent",
    "notifications",
    "translator_type"
  )

  val lots_interned: IList[(String, String)] =
    keys_lots_interned.map(s => (s, s))
  // these strings won't reference static strings
  val keys_lots_gen: IList[String] = keys_lots_interned.map(s => ("_" + s))
  // and these aren't even the same instances
  val keys_lots_gen_alt: IList[String] = keys_lots_interned.map(s => ("_" + s))

  val lots_gen: IList[(String, String)] = keys_lots_gen.map(s => (s, s))

  val lookup_lots_gen = createLots()

  val sized                               = 0.to(20).map(i => lots_gen.take(i)).toArray
  val ilists: Array[StringyIList[String]] = sized.map(new StringyIList(_))
  val hashes: Array[StringyMap[String]]   = sized.map(StringyMap(_))

  // for the lols
  val sizedLists = 0.to(20).map(i => lots_gen.take(i).toList).toArray
  val scalas: Array[Map[String, String]] =
    sizedLists.map(els => HashMap(els: _*))
  val scalazs: Array[String ==>> String] = sized.map(_.toMap)

  @Setup
  def setup(): Unit = {
    assert(lookupLots())

    assert(lookupIList01())
    assert(lookupIList02())
    assert(lookupIList03())
    assert(lookupIList04())
    assert(lookupIList05())
    assert(lookupIList06())
    assert(lookupIList07())
    assert(lookupIList08())
    assert(lookupIList09())

    assert(lookup01())
    assert(lookupHash02())
    assert(lookupHash03())
    assert(lookupHash04())
    assert(lookupHash05())
    assert(lookupHash06())
    assert(lookupHash07())
    assert(lookupHash08())
    assert(lookupHash09())

    assert(lookupScala01())
    assert(lookupScala02())
    assert(lookupScala03())
    assert(lookupScala04())
    assert(lookupScala05())
    assert(lookupScala06())
    assert(lookupScala07())
    assert(lookupScala08())
    assert(lookupScala09())

    assert(lookupScalaz01())
    assert(lookupScalaz02())
    assert(lookupScalaz03())
    assert(lookupScalaz04())
    assert(lookupScalaz05())
    assert(lookupScalaz06())
    assert(lookupScalaz07())
    assert(lookupScalaz08())
    assert(lookupScalaz09())

  }

  @Benchmark
  def createLots(): StringyMap[String] = StringyMap(lots_gen)

  @Benchmark
  def create00(): StringyMap[String] = StringyMap(sized(0))
  @Benchmark
  def create01(): StringyMap[String] = StringyMap(sized(1))
  @Benchmark
  def createHash02(): StringyMap[String] = StringyMap(sized(2))
  @Benchmark
  def createHash03(): StringyMap[String] = StringyMap(sized(3))
  @Benchmark
  def createHash04(): StringyMap[String] = StringyMap(sized(4))
  @Benchmark
  def createHash05(): StringyMap[String] = StringyMap(sized(5))
  @Benchmark
  def createHash06(): StringyMap[String] = StringyMap(sized(6))
  @Benchmark
  def createHash07(): StringyMap[String] = StringyMap(sized(7))
  @Benchmark
  def createHash08(): StringyMap[String] = StringyMap(sized(8))
  @Benchmark
  def createHash09(): StringyMap[String] = StringyMap(sized(9))

  @Benchmark
  def createScala00(): Map[String, String] = HashMap(sizedLists(0): _*)
  @Benchmark
  def createScala01(): Map[String, String] = HashMap(sizedLists(1): _*)
  @Benchmark
  def createScala02(): Map[String, String] = HashMap(sizedLists(2): _*)
  @Benchmark
  def createScala03(): Map[String, String] = HashMap(sizedLists(3): _*)
  @Benchmark
  def createScala04(): Map[String, String] = HashMap(sizedLists(4): _*)
  @Benchmark
  def createScala05(): Map[String, String] = HashMap(sizedLists(5): _*)
  @Benchmark
  def createScala06(): Map[String, String] = HashMap(sizedLists(6): _*)
  @Benchmark
  def createScala07(): Map[String, String] = HashMap(sizedLists(7): _*)
  @Benchmark
  def createScala08(): Map[String, String] = HashMap(sizedLists(8): _*)
  @Benchmark
  def createScala09(): Map[String, String] = HashMap(sizedLists(9): _*)

  @Benchmark
  def createScalaz00(): String ==>> String = sized(0).toMap
  @Benchmark
  def createScalaz01(): String ==>> String = sized(1).toMap
  @Benchmark
  def createScalaz02(): String ==>> String = sized(2).toMap
  @Benchmark
  def createScalaz03(): String ==>> String = sized(3).toMap
  @Benchmark
  def createScalaz04(): String ==>> String = sized(4).toMap
  @Benchmark
  def createScalaz05(): String ==>> String = sized(5).toMap
  @Benchmark
  def createScalaz06(): String ==>> String = sized(6).toMap
  @Benchmark
  def createScalaz07(): String ==>> String = sized(7).toMap
  @Benchmark
  def createScalaz08(): String ==>> String = sized(8).toMap
  @Benchmark
  def createScalaz09(): String ==>> String = sized(9).toMap

  @Benchmark
  def lookupLots(): Boolean = lookup(lookup_lots_gen)

  @Benchmark
  def lookupIList01(): Boolean = lookup(ilists(1))
  @Benchmark
  def lookupIList02(): Boolean = lookup(ilists(2))
  @Benchmark
  def lookupIList03(): Boolean = lookup(ilists(3))
  @Benchmark
  def lookupIList04(): Boolean = lookup(ilists(4))
  @Benchmark
  def lookupIList05(): Boolean = lookup(ilists(5))
  @Benchmark
  def lookupIList06(): Boolean = lookup(ilists(6))
  @Benchmark
  def lookupIList07(): Boolean = lookup(ilists(7))
  @Benchmark
  def lookupIList08(): Boolean = lookup(ilists(8))
  @Benchmark
  def lookupIList09(): Boolean = lookup(ilists(9))

  @Benchmark
  def lookup01(): Boolean = lookup(hashes(1))
  @Benchmark
  def lookupHash02(): Boolean = lookup(hashes(2))
  @Benchmark
  def lookupHash03(): Boolean = lookup(hashes(3))
  @Benchmark
  def lookupHash04(): Boolean = lookup(hashes(4))
  @Benchmark
  def lookupHash05(): Boolean = lookup(hashes(5))
  @Benchmark
  def lookupHash06(): Boolean = lookup(hashes(6))
  @Benchmark
  def lookupHash07(): Boolean = lookup(hashes(7))
  @Benchmark
  def lookupHash08(): Boolean = lookup(hashes(8))
  @Benchmark
  def lookupHash09(): Boolean = lookup(hashes(9))

  @Benchmark
  def lookupScala01(): Boolean = lookup(scalas(1))
  @Benchmark
  def lookupScala02(): Boolean = lookup(scalas(2))
  @Benchmark
  def lookupScala03(): Boolean = lookup(scalas(3))
  @Benchmark
  def lookupScala04(): Boolean = lookup(scalas(4))
  @Benchmark
  def lookupScala05(): Boolean = lookup(scalas(5))
  @Benchmark
  def lookupScala06(): Boolean = lookup(scalas(6))
  @Benchmark
  def lookupScala07(): Boolean = lookup(scalas(7))
  @Benchmark
  def lookupScala08(): Boolean = lookup(scalas(8))
  @Benchmark
  def lookupScala09(): Boolean = lookup(scalas(9))

  @Benchmark
  def lookupScalaz01(): Boolean = lookup(scalazs(1))
  @Benchmark
  def lookupScalaz02(): Boolean = lookup(scalazs(2))
  @Benchmark
  def lookupScalaz03(): Boolean = lookup(scalazs(3))
  @Benchmark
  def lookupScalaz04(): Boolean = lookup(scalazs(4))
  @Benchmark
  def lookupScalaz05(): Boolean = lookup(scalazs(5))
  @Benchmark
  def lookupScalaz06(): Boolean = lookup(scalazs(6))
  @Benchmark
  def lookupScalaz07(): Boolean = lookup(scalazs(7))
  @Benchmark
  def lookupScalaz08(): Boolean = lookup(scalazs(8))
  @Benchmark
  def lookupScalaz09(): Boolean = lookup(scalazs(9))

  @inline private final def lookup(sm: StringyMap[String]): Boolean =
    sm.get("_id").isDefined && !sm.get("_not_here_").isDefined
  @inline private final def lookup(sm: StringyIList[String]): Boolean =
    sm.get("_id").isDefined && !sm.get("_not_here_").isDefined
  @inline private final def lookup(sm: Map[String, String]): Boolean =
    sm.get("_id").isDefined && !sm.get("_not_here_").isDefined
  @inline private final def lookup(sm: String ==>> String): Boolean =
    sm.lookup("_id").isDefined && !sm.lookup("_not_here_").isDefined

}

// old implementations that have been shown to not be worth it...

final class StringyIList[A >: Null](
  private[this] val entries: IList[(String, A)]
) {
  def get(s: String): Maybe[A] = Maybe.fromNullable(find(entries, s))
  @tailrec private[this] final def find(
    rem: IList[(String, A)],
    s: String
  ): A =
    rem match {
      case _: INil[_] => null
      case c: ICons[_] =>
        if (c.head._1 == s) c.head._2
        else find(c.tail, s)
    }

}
