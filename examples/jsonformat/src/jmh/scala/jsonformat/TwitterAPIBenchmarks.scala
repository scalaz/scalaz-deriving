/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

// Copyright 2018 Andriy Plokhotnyuk

package jsonformat.benchmarks

import fommil.DerivedEqual
import jsonformat.*
import jsonformat.BenchmarkUtils.getResourceAsString
import jsonformat.JsDecoder.ops.*
import jsonformat.JsEncoder.ops.*
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State
import scalaz.*
import scalaz.Scalaz.*
import scalaz.annotation.deriving

// jsonformat/jmh:run -i 5 -wi 5 -f1 -t2 -w1 -r1 TwitterAPIBenchmarks.*
//
// see GoogleMapsAPIBenchmarks for profiling instructions

// reference for the format of tweets: https://developer.twitter.com/en/docs/tweets/search/api-reference/get-search-tweets.html

package m {
  @deriving(JsEncoder, JsDecoder)
  case class Urls(
    url: String,
    expanded_url: String,
    display_url: String,
    indices: IList[Int]
  )

  @deriving(JsEncoder, JsDecoder)
  case class Url(urls: IList[Urls])

  @deriving(JsEncoder, JsDecoder)
  case class UserEntities(url: Url, description: Url)

  @deriving(JsEncoder, JsDecoder)
  case class UserMentions(
    screen_name: String,
    name: String,
    id: Long,
    id_str: String,
    indices: IList[Int]
  )

  @deriving(JsEncoder, JsDecoder)
  case class User(
    id: Long,
    id_str: String,
    name: String,
    screen_name: String,
    location: String,
    description: String,
    url: String,
    entities: UserEntities,
    `protected`: Boolean,
    followers_count: Int,
    friends_count: Int,
    listed_count: Int,
    created_at: String,
    favourites_count: Int,
    utc_offset: Int,
    time_zone: String,
    geo_enabled: Boolean,
    verified: Boolean,
    statuses_count: Int,
    lang: String,
    contributors_enabled: Boolean,
    is_translator: Boolean,
    is_translation_enabled: Boolean,
    profile_background_color: String,
    profile_background_image_url: String,
    profile_background_image_url_https: String,
    profile_background_tile: Boolean,
    profile_image_url: String,
    profile_image_url_https: String,
    profile_banner_url: String,
    profile_link_color: String,
    profile_sidebar_border_color: String,
    profile_sidebar_fill_color: String,
    profile_text_color: String,
    profile_use_background_image: Boolean,
    has_extended_profile: Boolean,
    default_profile: Boolean,
    default_profile_image: Boolean,
    following: Boolean,
    follow_request_sent: Boolean,
    notifications: Boolean,
    translator_type: String
  )

  @deriving(JsEncoder, JsDecoder)
  case class Entities(
    hashtags: IList[String],
    symbols: IList[String],
    user_mentions: IList[UserMentions],
    urls: IList[Urls]
  )

  @deriving(JsEncoder, JsDecoder)
  case class RetweetedStatus(
    created_at: String,
    id: Long,
    id_str: String,
    text: String,
    truncated: Boolean,
    entities: Entities,
    source: String,
    in_reply_to_status_id: Option[String],
    in_reply_to_status_id_str: Option[String],
    in_reply_to_user_id: Option[String],
    in_reply_to_user_id_str: Option[String],
    in_reply_to_screen_name: Option[String],
    user: User,
    geo: Option[String],
    coordinates: Option[String],
    place: Option[String],
    contributors: Option[String],
    is_quote_status: Boolean,
    retweet_count: Int,
    favorite_count: Int,
    favorited: Boolean,
    retweeted: Boolean,
    possibly_sensitive: Boolean,
    lang: String
  )

  @deriving(JsEncoder, JsDecoder)
  case class Tweet(
    created_at: String,
    id: Long,
    id_str: String,
    text: String,
    truncated: Boolean,
    entities: Entities,
    source: String,
    in_reply_to_status_id: Option[String],
    in_reply_to_status_id_str: Option[String],
    in_reply_to_user_id: Option[String],
    in_reply_to_user_id_str: Option[String],
    in_reply_to_screen_name: Option[String],
    user: User,
    geo: Option[String],
    coordinates: Option[String],
    place: Option[String],
    contributors: Option[String],
    retweeted_status: RetweetedStatus,
    is_quote_status: Boolean,
    retweet_count: Int,
    favorite_count: Int,
    favorited: Boolean,
    retweeted: Boolean,
    possibly_sensitive: Boolean,
    lang: String
  )

  object Urls {
    implicit val equal: Equal[Urls] = MagnoliaEqual.gen
  }
  object Url {
    implicit val equal: Equal[Url] = MagnoliaEqual.gen
  }
  object UserEntities {
    implicit val equal: Equal[UserEntities] = MagnoliaEqual.gen
  }
  object UserMentions {
    implicit val equal: Equal[UserMentions] = MagnoliaEqual.gen
  }
  object User {
    implicit val equal: Equal[User] = MagnoliaEqual.gen
  }
  object Entities {
    implicit val equal: Equal[Entities] = MagnoliaEqual.gen
  }
  object RetweetedStatus {
    implicit val equal: Equal[RetweetedStatus] = MagnoliaEqual.gen
  }
  object Tweet {
    implicit val equal: Equal[Tweet] = MagnoliaEqual.gen
  }

}

package s {
  case class Urls(
    url: String,
    expanded_url: String,
    display_url: String,
    indices: IList[Int]
  )
  case class Url(urls: IList[Urls])
  case class UserEntities(url: Url, description: Url)
  case class UserMentions(
    screen_name: String,
    name: String,
    id: Long,
    id_str: String,
    indices: IList[Int]
  )
  case class User(
    id: Long,
    id_str: String,
    name: String,
    screen_name: String,
    location: String,
    description: String,
    url: String,
    entities: UserEntities,
    `protected`: Boolean,
    followers_count: Int,
    friends_count: Int,
    listed_count: Int,
    created_at: String,
    favourites_count: Int,
    utc_offset: Int,
    time_zone: String,
    geo_enabled: Boolean,
    verified: Boolean,
    statuses_count: Int,
    lang: String,
    contributors_enabled: Boolean,
    is_translator: Boolean,
    is_translation_enabled: Boolean,
    profile_background_color: String,
    profile_background_image_url: String,
    profile_background_image_url_https: String,
    profile_background_tile: Boolean,
    profile_image_url: String,
    profile_image_url_https: String,
    profile_banner_url: String,
    profile_link_color: String,
    profile_sidebar_border_color: String,
    profile_sidebar_fill_color: String,
    profile_text_color: String,
    profile_use_background_image: Boolean,
    has_extended_profile: Boolean,
    default_profile: Boolean,
    default_profile_image: Boolean,
    following: Boolean,
    follow_request_sent: Boolean,
    notifications: Boolean,
    translator_type: String
  )
  case class Entities(
    hashtags: IList[String],
    symbols: IList[String],
    user_mentions: IList[UserMentions],
    urls: IList[Urls]
  )
  case class RetweetedStatus(
    created_at: String,
    id: Long,
    id_str: String,
    text: String,
    truncated: Boolean,
    entities: Entities,
    source: String,
    in_reply_to_status_id: Option[String],
    in_reply_to_status_id_str: Option[String],
    in_reply_to_user_id: Option[String],
    in_reply_to_user_id_str: Option[String],
    in_reply_to_screen_name: Option[String],
    user: User,
    geo: Option[String],
    coordinates: Option[String],
    place: Option[String],
    contributors: Option[String],
    is_quote_status: Boolean,
    retweet_count: Int,
    favorite_count: Int,
    favorited: Boolean,
    retweeted: Boolean,
    possibly_sensitive: Boolean,
    lang: String
  )
  case class Tweet(
    created_at: String,
    id: Long,
    id_str: String,
    text: String,
    truncated: Boolean,
    entities: Entities,
    source: String,
    in_reply_to_status_id: Option[String],
    in_reply_to_status_id_str: Option[String],
    in_reply_to_user_id: Option[String],
    in_reply_to_user_id_str: Option[String],
    in_reply_to_screen_name: Option[String],
    user: User,
    geo: Option[String],
    coordinates: Option[String],
    place: Option[String],
    contributors: Option[String],
    retweeted_status: RetweetedStatus,
    is_quote_status: Boolean,
    retweet_count: Int,
    favorite_count: Int,
    favorited: Boolean,
    retweeted: Boolean,
    possibly_sensitive: Boolean,
    lang: String
  )

  object Urls {
    implicit val encoder: JsEncoder[Urls] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Urls] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[Urls] = DerivedEqual.gen
  }
  object Url {
    implicit val encoder: JsEncoder[Url] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Url] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[Url] = DerivedEqual.gen
  }
  object UserEntities {
    implicit val encoder: JsEncoder[UserEntities] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[UserEntities] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[UserEntities] = DerivedEqual.gen
  }
  object UserMentions {
    implicit val encoder: JsEncoder[UserMentions] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[UserMentions] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[UserMentions] = DerivedEqual.gen
  }
  object User {
    implicit val encoder: JsEncoder[User] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[User] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[User] = DerivedEqual.gen
  }
  object Entities {
    implicit val encoder: JsEncoder[Entities] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Entities] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[Entities] = DerivedEqual.gen
  }
  object RetweetedStatus {
    implicit val encoder: JsEncoder[RetweetedStatus] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[RetweetedStatus] =
      DerivedProductJsDecoder.gen
    implicit val equal: Equal[RetweetedStatus] = DerivedEqual.gen
  }
  object Tweet {
    implicit val encoder: JsEncoder[Tweet] = DerivedJsEncoder.gen
    implicit val decoder: JsDecoder[Tweet] = DerivedProductJsDecoder.gen
    implicit val equal: Equal[Tweet] = DerivedEqual.gen
  }
}

package z {
  @deriving(JsDecoder)
  case class Urls(
    url: String,
    expanded_url: String,
    display_url: String,
    indices: IList[Int]
  )

  @deriving(JsDecoder)
  case class Url(urls: IList[Urls])

  @deriving(JsDecoder)
  case class UserEntities(url: Url, description: Url)

  @deriving(JsDecoder)
  case class UserMentions(
    screen_name: String,
    name: String,
    id: Long,
    id_str: String,
    indices: IList[Int]
  )

  @deriving(JsDecoder)
  case class User(
    id: Long,
    id_str: String,
    name: String,
    screen_name: String,
    location: String,
    description: String,
    url: String,
    entities: UserEntities,
    `protected`: Boolean,
    followers_count: Int,
    friends_count: Int,
    listed_count: Int,
    created_at: String,
    favourites_count: Int,
    utc_offset: Int,
    time_zone: String,
    geo_enabled: Boolean,
    verified: Boolean,
    statuses_count: Int,
    lang: String,
    contributors_enabled: Boolean,
    is_translator: Boolean,
    is_translation_enabled: Boolean,
    profile_background_color: String,
    profile_background_image_url: String,
    profile_background_image_url_https: String,
    profile_background_tile: Boolean,
    profile_image_url: String,
    profile_image_url_https: String,
    profile_banner_url: String,
    profile_link_color: String,
    profile_sidebar_border_color: String,
    profile_sidebar_fill_color: String,
    profile_text_color: String,
    profile_use_background_image: Boolean,
    has_extended_profile: Boolean,
    default_profile: Boolean,
    default_profile_image: Boolean,
    following: Boolean,
    follow_request_sent: Boolean,
    notifications: Boolean,
    translator_type: String
  )

  @deriving(JsDecoder)
  case class Entities(
    hashtags: IList[String],
    symbols: IList[String],
    user_mentions: IList[UserMentions],
    urls: IList[Urls]
  )

  @deriving(JsDecoder)
  case class RetweetedStatus(
    created_at: String,
    id: Long,
    id_str: String,
    text: String,
    truncated: Boolean,
    entities: Entities,
    source: String,
    in_reply_to_status_id: Option[String],
    in_reply_to_status_id_str: Option[String],
    in_reply_to_user_id: Option[String],
    in_reply_to_user_id_str: Option[String],
    in_reply_to_screen_name: Option[String],
    user: User,
    geo: Option[String],
    coordinates: Option[String],
    place: Option[String],
    contributors: Option[String],
    is_quote_status: Boolean,
    retweet_count: Int,
    favorite_count: Int,
    favorited: Boolean,
    retweeted: Boolean,
    possibly_sensitive: Boolean,
    lang: String
  )

  @deriving(JsDecoder)
  case class Tweet(
    created_at: String,
    id: Long,
    id_str: String,
    text: String,
    truncated: Boolean,
    entities: Entities,
    source: String,
    in_reply_to_status_id: Option[String],
    in_reply_to_status_id_str: Option[String],
    in_reply_to_user_id: Option[String],
    in_reply_to_user_id_str: Option[String],
    in_reply_to_screen_name: Option[String],
    user: User,
    geo: Option[String],
    coordinates: Option[String],
    place: Option[String],
    contributors: Option[String],
    retweeted_status: RetweetedStatus,
    is_quote_status: Boolean,
    retweet_count: Int,
    favorite_count: Int,
    favorited: Boolean,
    retweeted: Boolean,
    possibly_sensitive: Boolean,
    lang: String
  )

  object Urls {
    implicit val equal: Equal[Urls] = Deriving.gen[Equal, Urls]
  }
  object Url {
    implicit val equal: Equal[Url] = Deriving.gen[Equal, Url]
  }
  object UserEntities {
    implicit val equal: Equal[UserEntities] = Deriving.gen[Equal, UserEntities]
  }
  object UserMentions {
    implicit val equal: Equal[UserMentions] = Deriving.gen[Equal, UserMentions]
  }
  object User {
    implicit val equal: Equal[User] = Deriving.gen[Equal, User]
  }
  object Entities {
    implicit val equal: Equal[Entities] = Deriving.gen[Equal, Entities]
  }
  object RetweetedStatus {
    implicit val equal: Equal[RetweetedStatus] =
      Deriving.gen[Equal, RetweetedStatus]
  }
  object Tweet {
    implicit val equal: Equal[Tweet] = Deriving.gen[Equal, Tweet]
  }

}

package h {
  case class Urls(
    url: String,
    expanded_url: String,
    display_url: String,
    indices: IList[Int]
  )
  case class Url(urls: IList[Urls])
  case class UserEntities(url: Url, description: Url)
  case class UserMentions(
    screen_name: String,
    name: String,
    id: Long,
    id_str: String,
    indices: IList[Int]
  )
  case class User(
    id: Long,
    id_str: String,
    name: String,
    screen_name: String,
    location: String,
    description: String,
    url: String,
    entities: UserEntities,
    `protected`: Boolean,
    followers_count: Int,
    friends_count: Int,
    listed_count: Int,
    created_at: String,
    favourites_count: Int,
    utc_offset: Int,
    time_zone: String,
    geo_enabled: Boolean,
    verified: Boolean,
    statuses_count: Int,
    lang: String,
    contributors_enabled: Boolean,
    is_translator: Boolean,
    is_translation_enabled: Boolean,
    profile_background_color: String,
    profile_background_image_url: String,
    profile_background_image_url_https: String,
    profile_background_tile: Boolean,
    profile_image_url: String,
    profile_image_url_https: String,
    profile_banner_url: String,
    profile_link_color: String,
    profile_sidebar_border_color: String,
    profile_sidebar_fill_color: String,
    profile_text_color: String,
    profile_use_background_image: Boolean,
    has_extended_profile: Boolean,
    default_profile: Boolean,
    default_profile_image: Boolean,
    following: Boolean,
    follow_request_sent: Boolean,
    notifications: Boolean,
    translator_type: String
  )
  case class Entities(
    hashtags: IList[String],
    symbols: IList[String],
    user_mentions: IList[UserMentions],
    urls: IList[Urls]
  )
  case class RetweetedStatus(
    created_at: String,
    id: Long,
    id_str: String,
    text: String,
    truncated: Boolean,
    entities: Entities,
    source: String,
    in_reply_to_status_id: Option[String],
    in_reply_to_status_id_str: Option[String],
    in_reply_to_user_id: Option[String],
    in_reply_to_user_id_str: Option[String],
    in_reply_to_screen_name: Option[String],
    user: User,
    geo: Option[String],
    coordinates: Option[String],
    place: Option[String],
    contributors: Option[String],
    is_quote_status: Boolean,
    retweet_count: Int,
    favorite_count: Int,
    favorited: Boolean,
    retweeted: Boolean,
    possibly_sensitive: Boolean,
    lang: String
  )
  case class Tweet(
    created_at: String,
    id: Long,
    id_str: String,
    text: String,
    truncated: Boolean,
    entities: Entities,
    source: String,
    in_reply_to_status_id: Option[String],
    in_reply_to_status_id_str: Option[String],
    in_reply_to_user_id: Option[String],
    in_reply_to_user_id_str: Option[String],
    in_reply_to_screen_name: Option[String],
    user: User,
    geo: Option[String],
    coordinates: Option[String],
    place: Option[String],
    contributors: Option[String],
    retweeted_status: RetweetedStatus,
    is_quote_status: Boolean,
    retweet_count: Int,
    favorite_count: Int,
    favorited: Boolean,
    retweeted: Boolean,
    possibly_sensitive: Boolean,
    lang: String
  )

  // \([^ ]+\):.*    "\1" -> a.\1.toJson ::
  // "\(.*\)".*      \1 <- j.getAs[]("\1")
  object Urls {
    implicit val encoder: JsEncoder[Urls] = a =>
      JsObject(
        "url" -> a.url.toJson ::
          "expanded_url" -> a.expanded_url.toJson ::
          "display_url" -> a.display_url.toJson ::
          "indices" -> a.indices.toJson ::
          IList.empty
      )
    implicit val decoder: JsDecoder[Urls] = JsDecoder.obj(4)(j =>
      for {
        url <- j.getAs[String]("url")
        expanded_url <- j.getAs[String]("expanded_url")
        display_url <- j.getAs[String]("display_url")
        indices <- j.getAs[IList[Int]]("indices")
      } yield Urls(url, expanded_url, display_url, indices)
    )

    implicit val equal: Equal[Urls] = (a1, a2) =>
      a1.url === a2.url &&
        a1.expanded_url === a2.expanded_url &&
        a1.display_url === a2.display_url
  }
  object Url {
    implicit val encoder: JsEncoder[Url] = a =>
      JsObject(
        "urls" -> a.urls.toJson ::
          IList.empty
      )
    implicit val decoder: JsDecoder[Url] = JsDecoder.obj(1)(j =>
      for {
        urls <- j.getAs[IList[Urls]]("urls")
      } yield Url(urls)
    )
    implicit val equal: Equal[Url] = (a1, a2) => a1.urls === a2.urls
  }
  object UserEntities {
    implicit val encoder: JsEncoder[UserEntities] = a =>
      JsObject(
        "url" -> a.url.toJson ::
          "description" -> a.description.toJson ::
          IList.empty
      )
    implicit val decoder: JsDecoder[UserEntities] = JsDecoder.obj(2)(j =>
      for {
        url <- j.getAs[Url]("url")
        description <- j.getAs[Url]("description")
      } yield UserEntities(url, description)
    )
    implicit val equal: Equal[UserEntities] = (a1, a2) =>
      a1.url === a2.url &&
        a1.description === a2.description
  }
  object UserMentions {
    implicit val encoder: JsEncoder[UserMentions] = a =>
      JsObject(
        "screen_name" -> a.screen_name.toJson ::
          "name" -> a.name.toJson ::
          "id" -> a.id.toJson ::
          "id_str" -> a.id_str.toJson ::
          "indices" -> a.indices.toJson ::
          IList.empty
      )
    implicit val decoder: JsDecoder[UserMentions] = JsDecoder.obj(5)(j =>
      for {
        screen_name <- j.getAs[String]("screen_name")
        name <- j.getAs[String]("name")
        id <- j.getAs[Long]("id")
        id_str <- j.getAs[String]("id_str")
        indices <- j.getAs[IList[Int]]("indices")
      } yield UserMentions(screen_name, name, id, id_str, indices)
    )
    implicit val equal: Equal[UserMentions] = (a1, a2) =>
      a1.screen_name === a2.screen_name &&
        a1.name === a2.name &&
        a1.id === a2.id &&
        a1.id_str === a2.id_str &&
        a1.indices === a2.indices

  }
  object User {
    implicit val encoder: JsEncoder[User] = a =>
      JsObject(
        "id" -> a.id.toJson ::
          "id_str" -> a.id_str.toJson ::
          "name" -> a.name.toJson ::
          "screen_name" -> a.screen_name.toJson ::
          "location" -> a.location.toJson ::
          "description" -> a.description.toJson ::
          "url" -> a.url.toJson ::
          "entities" -> a.entities.toJson ::
          "protected" -> a.`protected`.toJson ::
          "followers_count" -> a.followers_count.toJson ::
          "friends_count" -> a.friends_count.toJson ::
          "listed_count" -> a.listed_count.toJson ::
          "created_at" -> a.created_at.toJson ::
          "favourites_count" -> a.favourites_count.toJson ::
          "utc_offset" -> a.utc_offset.toJson ::
          "time_zone" -> a.time_zone.toJson ::
          "geo_enabled" -> a.geo_enabled.toJson ::
          "verified" -> a.verified.toJson ::
          "statuses_count" -> a.statuses_count.toJson ::
          "lang" -> a.lang.toJson ::
          "contributors_enabled" -> a.contributors_enabled.toJson ::
          "is_translator" -> a.is_translator.toJson ::
          "is_translation_enabled" -> a.is_translation_enabled.toJson ::
          "profile_background_color" -> a.profile_background_color.toJson ::
          "profile_background_image_url" -> a.profile_background_image_url.toJson ::
          "profile_background_image_url_https" -> a.profile_background_image_url_https.toJson ::
          "profile_background_tile" -> a.profile_background_tile.toJson ::
          "profile_image_url" -> a.profile_image_url.toJson ::
          "profile_image_url_https" -> a.profile_image_url_https.toJson ::
          "profile_banner_url" -> a.profile_banner_url.toJson ::
          "profile_link_color" -> a.profile_link_color.toJson ::
          "profile_sidebar_border_color" -> a.profile_sidebar_border_color.toJson ::
          "profile_sidebar_fill_color" -> a.profile_sidebar_fill_color.toJson ::
          "profile_text_color" -> a.profile_text_color.toJson ::
          "profile_use_background_image" -> a.profile_use_background_image.toJson ::
          "has_extended_profile" -> a.has_extended_profile.toJson ::
          "default_profile" -> a.default_profile.toJson ::
          "default_profile_image" -> a.default_profile_image.toJson ::
          "following" -> a.following.toJson ::
          "follow_request_sent" -> a.follow_request_sent.toJson ::
          "notifications" -> a.notifications.toJson ::
          "translator_type" -> a.translator_type.toJson ::
          IList.empty
      )
    implicit val decoder: JsDecoder[User] = JsDecoder.obj(41)(j =>
      for {
        id <- j.getAs[Long]("id")
        id_str <- j.getAs[String]("id_str")
        name <- j.getAs[String]("name")
        screen_name <- j.getAs[String]("screen_name")
        location <- j.getAs[String]("location")
        description <- j.getAs[String]("description")
        url <- j.getAs[String]("url")
        entities <- j.getAs[UserEntities]("entities")
        protected_ <- j.getAs[Boolean]("protected")
        followers_count <- j.getAs[Int]("followers_count")
        friends_count <- j.getAs[Int]("friends_count")
        listed_count <- j.getAs[Int]("listed_count")
        created_at <- j.getAs[String]("created_at")
        favourites_count <- j.getAs[Int]("favourites_count")
        utc_offset <- j.getAs[Int]("utc_offset")
        time_zone <- j.getAs[String]("time_zone")
        geo_enabled <- j.getAs[Boolean]("geo_enabled")
        verified <- j.getAs[Boolean]("verified")
        statuses_count <- j.getAs[Int]("statuses_count")
        lang <- j.getAs[String]("lang")
        contributors_enabled <- j.getAs[Boolean]("contributors_enabled")
        is_translator <- j.getAs[Boolean]("is_translator")
        is_translation_enabled <- j.getAs[Boolean]("is_translation_enabled")
        profile_background_color <- j.getAs[String](
          "profile_background_color"
        )
        profile_background_image_url <- j.getAs[String](
          "profile_background_image_url"
        )
        profile_background_image_url_https <-
          j.getAs[String](
            "profile_background_image_url_https"
          )
        profile_background_tile <- j.getAs[Boolean]("profile_background_tile")
        profile_image_url <- j.getAs[String]("profile_image_url")
        profile_image_url_https <- j.getAs[String]("profile_image_url_https")
        profile_banner_url <- j.getAs[String]("profile_banner_url")
        profile_link_color <- j.getAs[String]("profile_link_color")
        profile_sidebar_border_color <- j.getAs[String](
          "profile_sidebar_border_color"
        )
        profile_sidebar_fill_color <- j.getAs[String](
          "profile_sidebar_fill_color"
        )
        profile_text_color <- j.getAs[String]("profile_text_color")
        profile_use_background_image <- j.getAs[Boolean](
          "profile_use_background_image"
        )
        has_extended_profile <- j.getAs[Boolean]("has_extended_profile")
        default_profile <- j.getAs[Boolean]("default_profile")
        default_profile_image <- j.getAs[Boolean]("default_profile_image")
        following <- j.getAs[Boolean]("following")
        follow_request_sent <- j.getAs[Boolean]("follow_request_sent")
        notifications <- j.getAs[Boolean]("notifications")
        translator_type <- j.getAs[String]("translator_type")
      } yield User(
        id,
        id_str,
        name,
        screen_name,
        location,
        description,
        url,
        entities,
        protected_,
        followers_count,
        friends_count,
        listed_count,
        created_at,
        favourites_count,
        utc_offset,
        time_zone,
        geo_enabled,
        verified,
        statuses_count,
        lang,
        contributors_enabled,
        is_translator,
        is_translation_enabled,
        profile_background_color,
        profile_background_image_url,
        profile_background_image_url_https,
        profile_background_tile,
        profile_image_url,
        profile_image_url_https,
        profile_banner_url,
        profile_link_color,
        profile_sidebar_border_color,
        profile_sidebar_fill_color,
        profile_text_color,
        profile_use_background_image,
        has_extended_profile,
        default_profile,
        default_profile_image,
        following,
        follow_request_sent,
        notifications,
        translator_type
      )
    )
    implicit val equal: Equal[User] = (a1, a2) =>
      a1.id === a2.id &&
        a1.id_str === a2.id_str &&
        a1.name === a2.name &&
        a1.screen_name === a2.screen_name &&
        a1.location === a2.location &&
        a1.description === a2.description &&
        a1.url === a2.url &&
        a1.entities === a2.entities &&
        a1.`protected` === a2.`protected` &&
        a1.followers_count === a2.followers_count &&
        a1.friends_count === a2.friends_count &&
        a1.listed_count === a2.listed_count &&
        a1.created_at === a2.created_at &&
        a1.favourites_count === a2.favourites_count &&
        a1.utc_offset === a2.utc_offset &&
        a1.time_zone === a2.time_zone &&
        a1.geo_enabled === a2.geo_enabled &&
        a1.verified === a2.verified &&
        a1.statuses_count === a2.statuses_count &&
        a1.lang === a2.lang &&
        a1.contributors_enabled === a2.contributors_enabled &&
        a1.is_translator === a2.is_translator &&
        a1.is_translation_enabled === a2.is_translation_enabled &&
        a1.profile_background_color === a2.profile_background_color &&
        a1.profile_background_image_url === a2.profile_background_image_url &&
        a1.profile_background_image_url_https === a2.profile_background_image_url_https &&
        a1.profile_background_tile === a2.profile_background_tile &&
        a1.profile_image_url === a2.profile_image_url &&
        a1.profile_image_url_https === a2.profile_image_url_https &&
        a1.profile_banner_url === a2.profile_banner_url &&
        a1.profile_link_color === a2.profile_link_color &&
        a1.profile_sidebar_border_color === a2.profile_sidebar_border_color &&
        a1.profile_sidebar_fill_color === a2.profile_sidebar_fill_color &&
        a1.profile_text_color === a2.profile_text_color &&
        a1.profile_use_background_image === a2.profile_use_background_image &&
        a1.has_extended_profile === a2.has_extended_profile &&
        a1.default_profile === a2.default_profile &&
        a1.default_profile_image === a2.default_profile_image &&
        a1.following === a2.following &&
        a1.follow_request_sent === a2.follow_request_sent &&
        a1.notifications === a2.notifications &&
        a1.translator_type === a2.translator_type
  }
  object Entities {
    implicit val encoder: JsEncoder[Entities] = a =>
      JsObject(
        "hashtags" -> a.hashtags.toJson ::
          "symbols" -> a.symbols.toJson ::
          "user_mentions" -> a.user_mentions.toJson ::
          "urls" -> a.urls.toJson ::
          IList.empty
      )
    implicit val decoder: JsDecoder[Entities] = JsDecoder.obj(4)(j =>
      for {
        hashtags <- j.getAs[IList[String]]("hashtags")
        symbols <- j.getAs[IList[String]]("symbols")
        user_mentions <- j.getAs[IList[UserMentions]]("user_mentions")
        urls <- j.getAs[IList[Urls]]("urls")
      } yield Entities(hashtags, symbols, user_mentions, urls)
    )
    implicit val equal: Equal[Entities] = (a1, a2) =>
      a1.hashtags === a2.hashtags &&
        a1.symbols === a2.symbols &&
        a1.user_mentions === a2.user_mentions &&
        a1.urls === a2.urls
  }
  object RetweetedStatus {
    def opt[A: JsEncoder](
      field: String,
      a: Option[A]
    ): IList[(String, JsValue)] =
      a match {
        case Some(a) => field -> a.toJson :: IList.empty
        case None    => IList.empty
      }

    implicit val encoder: JsEncoder[RetweetedStatus] = a =>
      JsObject(
        "created_at" -> a.created_at.toJson ::
          "id" -> a.id.toJson ::
          "id_str" -> a.id_str.toJson ::
          "text" -> a.text.toJson ::
          "truncated" -> a.truncated.toJson ::
          "entities" -> a.entities.toJson ::
          "source" -> a.source.toJson ::
          opt("in_reply_to_status_id", a.in_reply_to_status_id) :::
          opt("in_reply_to_status_id_str", a.in_reply_to_status_id_str) :::
          opt("in_reply_to_user_id", a.in_reply_to_user_id) :::
          opt("in_reply_to_user_id_str", a.in_reply_to_user_id_str) :::
          opt("in_reply_to_screen_name", a.in_reply_to_screen_name) :::
          "user" -> a.user.toJson ::
          opt("geo", a.geo) :::
          opt("coordinates", a.coordinates) :::
          opt("place", a.place) :::
          opt("contributors", a.contributors) :::
          "is_quote_status" -> a.is_quote_status.toJson ::
          "retweet_count" -> a.retweet_count.toJson ::
          "favorite_count" -> a.favorite_count.toJson ::
          "favorited" -> a.favorited.toJson ::
          "retweeted" -> a.retweeted.toJson ::
          "possibly_sensitive" -> a.possibly_sensitive.toJson ::
          "lang" -> a.lang.toJson ::
          IList.empty
      )
    implicit val decoder: JsDecoder[RetweetedStatus] = JsDecoder.obj(25)(j =>
      for {
        created_at <- j.getAs[String]("created_at")
        id <- j.getAs[Long]("id")
        id_str <- j.getAs[String]("id_str")
        text <- j.getAs[String]("text")
        truncated <- j.getAs[Boolean]("truncated")
        entities <- j.getAs[Entities]("entities")
        source <- j.getAs[String]("source")
        in_reply_to_status_id <- j.getOption[String](
          "in_reply_to_status_id"
        )
        in_reply_to_status_id_str <- j.getOption[String](
          "in_reply_to_status_id_str"
        )
        in_reply_to_user_id <- j.getOption[String]("in_reply_to_user_id")
        in_reply_to_user_id_str <- j.getOption[String](
          "in_reply_to_user_id_str"
        )
        in_reply_to_screen_name <- j.getOption[String](
          "in_reply_to_screen_name"
        )
        user <- j.getAs[User]("user")
        geo <- j.getOption[String]("geo")
        coordinates <- j.getOption[String]("coordinates")
        place <- j.getOption[String]("place")
        contributors <- j.getOption[String]("contributors")
        is_quote_status <- j.getAs[Boolean]("is_quote_status")
        retweet_count <- j.getAs[Int]("retweet_count")
        favorite_count <- j.getAs[Int]("favorite_count")
        favorited <- j.getAs[Boolean]("favorited")
        retweeted <- j.getAs[Boolean]("retweeted")
        possibly_sensitive <- j.getAs[Boolean]("possibly_sensitive")
        lang <- j.getAs[String]("lang")

      } yield RetweetedStatus(
        created_at,
        id,
        id_str,
        text,
        truncated,
        entities,
        source,
        in_reply_to_status_id,
        in_reply_to_status_id_str,
        in_reply_to_user_id,
        in_reply_to_user_id_str,
        in_reply_to_screen_name,
        user,
        geo,
        coordinates,
        place,
        contributors,
        is_quote_status,
        retweet_count,
        favorite_count,
        favorited,
        retweeted,
        possibly_sensitive,
        lang
      )
    )
    implicit val equal: Equal[RetweetedStatus] = (a1, a2) =>
      a1.created_at === a2.created_at &&
        a1.id === a2.id &&
        a1.id_str === a2.id_str &&
        a1.text === a2.text &&
        a1.truncated === a2.truncated &&
        a1.entities === a2.entities &&
        a1.source === a2.source &&
        a1.in_reply_to_status_id === a2.in_reply_to_status_id &&
        a1.in_reply_to_status_id_str === a2.in_reply_to_status_id_str &&
        a1.in_reply_to_user_id === a2.in_reply_to_user_id &&
        a1.in_reply_to_user_id_str === a2.in_reply_to_user_id_str &&
        a1.in_reply_to_screen_name === a2.in_reply_to_screen_name &&
        a1.user === a2.user &&
        a1.geo === a2.geo &&
        a1.coordinates === a2.coordinates &&
        a1.place === a2.place &&
        a1.contributors === a2.contributors &&
        a1.is_quote_status === a2.is_quote_status &&
        a1.retweet_count === a2.retweet_count &&
        a1.favorite_count === a2.favorite_count &&
        a1.favorited === a2.favorited &&
        a1.retweeted === a2.retweeted &&
        a1.possibly_sensitive === a2.possibly_sensitive &&
        a1.lang === a2.lang

  }
  object Tweet {
    def opt[A: JsEncoder](
      field: String,
      a: Option[A]
    ): IList[(String, JsValue)] =
      a match {
        case Some(a) => field -> a.toJson :: IList.empty
        case None    => IList.empty
      }

    implicit val encoder: JsEncoder[Tweet] = a =>
      JsObject(
        "created_at" -> a.created_at.toJson ::
          "id" -> a.id.toJson ::
          "id_str" -> a.id_str.toJson ::
          "text" -> a.text.toJson ::
          "truncated" -> a.truncated.toJson ::
          "entities" -> a.entities.toJson ::
          "source" -> a.source.toJson ::
          opt("in_reply_to_status_id", a.in_reply_to_status_id) :::
          opt("in_reply_to_status_id_str", a.in_reply_to_status_id_str) :::
          opt("in_reply_to_user_id", a.in_reply_to_user_id) :::
          opt("in_reply_to_user_id_str", a.in_reply_to_user_id_str) :::
          opt("in_reply_to_screen_name", a.in_reply_to_screen_name) :::
          "user" -> a.user.toJson ::
          opt("geo", a.geo) :::
          opt("coordinates", a.coordinates) :::
          opt("place", a.place) :::
          opt("contributors", a.contributors) :::
          "retweeted_status" -> a.retweeted_status.toJson ::
          "is_quote_status" -> a.is_quote_status.toJson ::
          "retweet_count" -> a.retweet_count.toJson ::
          "favorite_count" -> a.favorite_count.toJson ::
          "favorited" -> a.favorited.toJson ::
          "retweeted" -> a.retweeted.toJson ::
          "possibly_sensitive" -> a.possibly_sensitive.toJson ::
          "lang" -> a.lang.toJson ::
          IList.empty
      )
    implicit val decoder: JsDecoder[Tweet] = JsDecoder.obj(25)(j =>
      for {
        created_at <- j.getAs[String]("created_at")
        id <- j.getAs[Long]("id")
        id_str <- j.getAs[String]("id_str")
        text <- j.getAs[String]("text")
        truncated <- j.getAs[Boolean]("truncated")
        entities <- j.getAs[Entities]("entities")
        source <- j.getAs[String]("source")
        in_reply_to_status_id <- j.getOption[String](
          "in_reply_to_status_id"
        )
        in_reply_to_status_id_str <- j.getOption[String](
          "in_reply_to_status_id_str"
        )
        in_reply_to_user_id <- j.getOption[String]("in_reply_to_user_id")
        in_reply_to_user_id_str <- j.getOption[String](
          "in_reply_to_user_id_str"
        )
        in_reply_to_screen_name <- j.getOption[String](
          "in_reply_to_screen_name"
        )
        user <- j.getAs[User]("user")
        geo <- j.getOption[String]("geo")
        coordinates <- j.getOption[String]("coordinates")
        place <- j.getOption[String]("place")
        contributors <- j.getOption[String]("contributors")
        retweeted_status <- j.getAs[RetweetedStatus]("retweeted_status")
        is_quote_status <- j.getAs[Boolean]("is_quote_status")
        retweet_count <- j.getAs[Int]("retweet_count")
        favorite_count <- j.getAs[Int]("favorite_count")
        favorited <- j.getAs[Boolean]("favorited")
        retweeted <- j.getAs[Boolean]("retweeted")
        possibly_sensitive <- j.getAs[Boolean]("possibly_sensitive")
        lang <- j.getAs[String]("lang")

      } yield Tweet(
        created_at,
        id,
        id_str,
        text,
        truncated,
        entities,
        source,
        in_reply_to_status_id,
        in_reply_to_status_id_str,
        in_reply_to_user_id,
        in_reply_to_user_id_str,
        in_reply_to_screen_name,
        user,
        geo,
        coordinates,
        place,
        contributors,
        retweeted_status,
        is_quote_status,
        retweet_count,
        favorite_count,
        favorited,
        retweeted,
        possibly_sensitive,
        lang
      )
    )
    implicit val equal: Equal[Tweet] = (a1, a2) =>
      a1.created_at === a2.created_at &&
        a1.id === a2.id &&
        a1.id_str === a2.id_str &&
        a1.text === a2.text &&
        a1.truncated === a2.truncated &&
        a1.entities === a2.entities &&
        a1.source === a2.source &&
        a1.in_reply_to_status_id === a2.in_reply_to_status_id &&
        a1.in_reply_to_status_id_str === a2.in_reply_to_status_id_str &&
        a1.in_reply_to_user_id === a2.in_reply_to_user_id &&
        a1.in_reply_to_user_id_str === a2.in_reply_to_user_id_str &&
        a1.in_reply_to_screen_name === a2.in_reply_to_screen_name &&
        a1.user === a2.user &&
        a1.geo === a2.geo &&
        a1.coordinates === a2.coordinates &&
        a1.place === a2.place &&
        a1.contributors === a2.contributors &&
        a1.retweeted_status === a2.retweeted_status &&
        a1.is_quote_status === a2.is_quote_status &&
        a1.retweet_count === a2.retweet_count &&
        a1.favorite_count === a2.favorite_count &&
        a1.favorited === a2.favorited &&
        a1.retweeted === a2.retweeted &&
        a1.possibly_sensitive === a2.possibly_sensitive &&
        a1.lang === a2.lang

  }
}

@State(Scope.Benchmark)
class TwitterAPIBenchmarks {
  var jsonString, jsonString2, jsonString3: String = _
  var ast1, ast2: JsValue = _

  var objm, objm_, objm__ : List[m.Tweet] = _
  var objs, objs_, objs__ : List[s.Tweet] = _
  var objh, objh_, objh__ : List[h.Tweet] = _
  var objz, objz_, objz__ : List[z.Tweet] = _

  @Setup
  def setup(): Unit = {
    jsonString = getResourceAsString("twitter_api_response.json")
    jsonString2 = getResourceAsString("twitter_api_compact_response.json")
    jsonString3 = getResourceAsString("twitter_api_error_response.json")
    ast1 = JsParser(jsonString).getOrElse(null)
    ast2 = JsParser(jsonString3).getOrElse(null)
    objm = decodeMagnoliaSuccess().getOrElse(null)
    require(CompactPrinter(encodeMagnolia()) == jsonString2)
    require(decodeMagnoliaError.isLeft)

    objs = decodeShapelessSuccess().getOrElse(null)
    require(CompactPrinter(encodeShapeless()) == jsonString2)
    require(decodeShapelessError().isLeft)

    objh = decodeManualSuccess().getOrElse(null)
    require(CompactPrinter(encodeManual()) == jsonString2)
    require(decodeManualError.isLeft)

    objz = decodeScalazSuccess().getOrElse(null)

    val len = objz.size
    // not reusing the same objects to avoid instance identity
    objm_ = decodeMagnoliaSuccess().getOrElse(null)
    objs_ = decodeShapelessSuccess().getOrElse(null)
    objh_ = decodeManualSuccess().getOrElse(null)
    objz_ = decodeScalazSuccess().getOrElse(null)
    objm__ = decodeMagnoliaSuccess().getOrElse(null).take(len - 1)
    objs__ = decodeShapelessSuccess().getOrElse(null).take(len - 1)
    objh__ = decodeManualSuccess().getOrElse(null).take(len - 1)
    objz__ = decodeScalazSuccess().getOrElse(null).take(len - 1)
  }

  @Benchmark
  def decodeMagnoliaSuccess(): \/[String, List[m.Tweet]] =
    ast1.as[List[m.Tweet]]

  @Benchmark
  def decodeMagnoliaError(): \/[String, List[m.Tweet]] =
    ast2.as[List[m.Tweet]]

  def decodeScalazSuccess(): \/[String, List[z.Tweet]] =
    ast1.as[List[z.Tweet]]

  @Benchmark
  def encodeMagnolia(): JsValue = objm.toJson

  @Benchmark
  def decodeShapelessSuccess(): \/[String, List[s.Tweet]] =
    ast1.as[List[s.Tweet]]

  @Benchmark
  def decodeShapelessError(): \/[String, List[s.Tweet]] =
    ast2.as[List[s.Tweet]]

  @Benchmark
  def encodeShapeless(): JsValue = objs.toJson

  @Benchmark
  def decodeManualSuccess(): \/[String, List[h.Tweet]] =
    ast1.as[List[h.Tweet]]

  @Benchmark
  def decodeManualError(): \/[String, List[h.Tweet]] =
    ast2.as[List[h.Tweet]]

  @Benchmark
  def encodeManual(): JsValue = objh.toJson

  @Benchmark
  def equalScalazTrue(): Boolean = objz === objz_

  @Benchmark
  def equalScalazFalse(): Boolean = objz === objz__

  @Benchmark
  def equalMagnoliaTrue(): Boolean = objm === objm_

  @Benchmark
  def equalMagnoliaFalse(): Boolean = objm === objm__

  @Benchmark
  def equalShapelessTrue(): Boolean = objs === objs_

  @Benchmark
  def equalShapelessFalse(): Boolean = objs === objs__

  @Benchmark
  def equalManualTrue(): Boolean = objh === objh_

  @Benchmark
  def equalManualFalse(): Boolean = objh === objh__

}
