// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package xmlformat.internal

import scala.annotation.tailrec
import scalaz._, Scalaz._

private[xmlformat] abstract class StringyMultiMap[A] {
  def get(s: String): IList[A]
  def find(s: String): Maybe[A] = get(s).headMaybe
}
private[xmlformat] object StringyMultiMap {
  def apply[A >: Null](
    entries: IList[A],
    expectedLookups: Int
  )(extract: A => String): StringyMultiMap[A] =
    apply(entries.map(a => (extract(a), a)), expectedLookups)

  def apply[A >: Null](
    entries: IList[(String, A)],
    expectedLookups: Int
  ): StringyMultiMap[A] = entries match {
    case INil() | ICons(_, INil()) => StringyMultiIList(entries)
    case _ if expectedLookups < 10 => StringyMultiIList(entries)
    case _ if expectedLookups == Int.MaxValue =>
      StringyMultiJavaMap(entries, 16)
    case _ =>
      // see the analysis in StringyMap for a justification
      @tailrec def poke(rem: IList[(String, A)], target: Int, acc: Int): Int =
        rem match {
          case _: INil[_] => acc
          case c: ICons[_] =>
            if (acc > target) -1
            else poke(c.tail, target, acc + 1)
        }
      val b                          = expectedLookups
      val ab                         = poke(entries, b, 0)
      def cost_ilist(a: Int): Double = a * b / 2.8
      def cost_jhash(a: Int): Double =
        if (a < 6) 1.0 / (0.57 - 0.166 * math.log(a.toDouble))
        else b / 2.8 + math.pow(a.toDouble, 1.05) / 1.8
      val a = if (ab < 0) entries.length else ab
      if (cost_ilist(a) < cost_jhash(a)) StringyMultiIList(entries)
      else StringyMultiJavaMap(entries, a)
  }
}

private[internal] final class StringyMultiJavaMap[A] private (
  private[this] val hashmap: java.util.HashMap[String, IList[A]]
) extends StringyMultiMap[A] {
  // scalafix:off
  def get(s: String): IList[A] = {
    val got = hashmap.get(s)
    if (got == null) IList.empty else got
  }
  // scalafix:on
}
private[internal] object StringyMultiJavaMap {
  def apply[A](
    entries: IList[(String, A)],
    size: Int
  ): StringyMultiJavaMap[A] = {
    val hashmap = new java.util.HashMap[String, IList[A]](size)
    @tailrec def visit(rem: IList[(String, A)]): Unit = rem match {
      case _: INil[_] => ()
      case c: ICons[_] =>
        val key   = c.head._1
        val value = c.head._2
        val old   = hashmap.get(key)
        if (old == null) // scalafix:ok
          hashmap.put(key, value :: IList.empty)
        else
          hashmap.put(key, value :: old)
        visit(c.tail)
    }
    visit(entries.reverse)
    new StringyMultiJavaMap(hashmap)
  }
}

private[internal] final class StringyMultiIList[A] private (
  entries: IList[(String, A)]
) extends StringyMultiMap[A] {
  def get(s: String): IList[A] = entries.filter(_._1 == s).map(_._2)
  override def find(s: String): Maybe[A] =
    entries.find(_._1 == s).map(_._2).toMaybe
}
private[internal] object StringyMultiIList {
  def apply[A](entries: IList[(String, A)]): StringyMultiIList[A] =
    new StringyMultiIList(entries)
}
