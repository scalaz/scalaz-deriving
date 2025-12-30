/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package jsonformat.internal

import scala.annotation.tailrec
import scalaz._

/**
 * Optimised String lookup. Provide the fastest aggregate creation and lookup
 * time for a fixed table of values indexed by Strings.
 *
 * Roughly speaking, Java HashMap is faster to create but slower to query than a
 * linked list structure. And there are edge cases.
 *
 * Design constraints are:
 *
 * 1. we don't know how many entries are to be indexed (without traversing).
 * 2  we don't know if there are dupes (first values should win).
 * 3. we have an estimate of how many queries are to be performed.
 */
sealed abstract class StringyMap[A]       {
  def get(s: String): Maybe[A]
}
object StringyMap                         {
  def apply[A >: Null](
    entries: IList[(String, A)],
    expectedLookups: Int
  ): StringyMap[A] =
    entries match {
      case INil() | ICons(_, INil())            => StringyIList(entries)
      // can probably special case more...
      case _ if expectedLookups < 10            => StringyIList(entries)
      case _ if expectedLookups == Int.MaxValue => StringyJavaMap(entries, 16)
      case _                                    =>
        // numbers from StringyMapBenchmarks
        // https://docs.google.com/spreadsheets/d/1sx6LlcYn0YXmvppE6Vy3RgLxZn8FYVeQ_wb0zeHaD7s
        // see stringy.R for 3d plots.
        //
        // scala's HashMap is very inefficient at both creation and lookup, so
        // is not considered.
        //
        // for a = size of entries
        //     b = lifetime lookups
        //
        // All cost estimates below are worst case. The actual values
        // (specific to my machine) are not as important as their relative
        // cost to each other. If this varies across machines and platforms
        // this entire analysis is bogus.
        //
        // java HashMap.
        // creation (a < 6) costs = 1 / (0.57 - 0.166 * ln(a))
        // creation (a > 6) costs = a ^ 1.05 / 1.8
        // lookup costs           = 1 / 2.8
        //
        // IList
        // creation costs = 0
        // lookup costs   = a / 2.8
        // NOTE: there are several variants depending on eq/hashCode/equals.
        //
        // To choose the most efficient, pick the minimum of: creation + b * lookup.
        //
        // However, this is further complicated by not knowing a, with a
        // (worst case) cost of a to compute a! We can calculate if a < b
        // with a cost of min(a, b).

        // if rem is longer than target, return -1.
        // if rem is shorter or the same length as target, return the length of rem.
        @tailrec def poke(rem: IList[(String, A)], target: Int, acc: Int): Int =
          rem match {
            case _: INil[_]  => acc
            case c: ICons[_] =>
              if (acc > target) -1
              else poke(c.tail, target, acc + 1)
          }
        val b                                                                  = expectedLookups
        val ab                                                                 = poke(entries, b, 0)

        def cost_ilist(a: Int): Double = a * b / 2.8
        def cost_jhash(a: Int): Double =
          if (a < 6) 1.0 / (0.57 - 0.166 * math.log(a.toDouble))
          else b / 2.8 + math.pow(a.toDouble, 1.05) / 1.8

        // this could be optimised
        val a = if (ab < 0) entries.length else ab
        if (cost_ilist(a) < cost_jhash(a)) StringyIList(entries)
        else StringyJavaMap(entries, a)
    }
}
// old implementations that have been shown to not be worth it...
private[jsonformat] final class StringyJavaMap[A] private (
  private[this] val hashmap: java.util.HashMap[String, A]
) extends StringyMap[A] {
  def get(s: String): Maybe[A] = Maybe.fromNullable(hashmap.get(s))
}
private[jsonformat] object StringyJavaMap {
  def apply[A](
    entries: IList[(String, A)],
    size: Int
  ): StringyJavaMap[A] = {
    val jhashmap                                      = new java.util.HashMap[String, A](size)
    @tailrec def visit(rem: IList[(String, A)]): Unit =
      rem match {
        case _: INil[_]  => ()
        case c: ICons[_] =>
          jhashmap.putIfAbsent(c.head._1, c.head._2)
          visit(c.tail)
      }
    visit(entries)
    new StringyJavaMap(jhashmap)
  }
}
private[jsonformat] final class StringyIList[A] private (
  private[this] val entries: IList[(String, A)]
) extends StringyMap[A] {
  def get(s: String): Maybe[A] = find(entries, s)
  @tailrec private[this] final def find(
    rem: IList[(String, A)],
    s: String
  ): Maybe[A]                  =
    rem match {
      case _: INil[_]  => Maybe.empty
      case c: ICons[_] =>
        // for small lookup numbers (e.g. one time traversals) it might be faster
        // not to compute the hashCodes.
        val key = c.head._1
        if ((key.eq(s)) || ((key.hashCode == s.hashCode) && (key == s)))
          Maybe.just(c.head._2)
        else
          find(c.tail, s)
    }
}
private[jsonformat] object StringyIList   {
  def apply[A](entries: IList[(String, A)]): StringyIList[A] =
    new StringyIList(entries)
}
