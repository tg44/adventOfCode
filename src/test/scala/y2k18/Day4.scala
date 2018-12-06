package y2k18

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

import scala.annotation.tailrec

class Day4 extends WordSpecLike with Matchers {

  implicit class ListHelper[A](list: List[A]) {
    def split(f: A => Boolean): List[List[A]] = {
      @tailrec
      def rec(rem: List[A], actual: List[A], accAll: List[List[A]]): List[List[A]] = {
        rem match {
          case Nil => actual :: accAll
          case h :: t if f(h) => rec(t, Nil, (h :: actual) :: accAll)
          case h :: t => rec(t, h :: actual, accAll)
        }
      }

      rec(list.reverse, List.empty, List.empty)
    }
  }

  def parseDailyLog(log: List[String]) = {
    val guardId = log.head.split("#")(1).split(" ")(0).toInt
    val sleepWakePairs = log.tail.split(_.contains("falls")).tail.filter(_.size == 2).map(_.map(_.split(":")(1).split("]")(0).toInt))
    val sleepedMinutes = sleepWakePairs.flatMap { sw =>
      sw.head until sw.last
    }
    (guardId, sleepedMinutes)
  }

  def d4s1(log: List[String]) = {
    //the first segment will be an empty string or junk...
    val best = log.sorted
      .split(_.contains("#")).tail
      .map(parseDailyLog)
      .groupBy(_._1).mapValues(_.flatMap(_._2))
      .filter(_._2.nonEmpty)
      .mapValues { minutes => (minutes.size, minutes.groupBy(identity).mapValues(_.size).maxBy(_._2)._1)}
      .maxBy(_._2._1)
    best._1 * best._2._2
  }

  def d4s2(log: List[String]) = {
    //the first segment will be an empty string or junk...
    val best = log.sorted
      .split(_.contains("#")).tail
      .map(parseDailyLog)
      .groupBy(_._1).mapValues(_.flatMap(_._2))
      .filter(_._2.nonEmpty)
      .mapValues { minutes => minutes.groupBy(identity).mapValues(_.size).maxBy(_._2) }
      .maxBy(_._2._2)
    best._1 * best._2._1
  }


  "listSplit" should {
    "work" in {
      List(true, false, false, true, true, false).split(identity) shouldBe List(List(), List(true, false, false), List(true), List(true, false))
    }
  }

  "parseDailyLog" in {
    parseDailyLog(
      List(
        "[1518-11-01 00:00] Guard #10 begins shift",
        "[1518-11-01 00:05] falls asleep",
        "[1518-11-01 00:07] wakes up",
        "[1518-11-01 00:30] falls asleep",
        "[1518-11-01 00:35] wakes up"
      )) shouldBe(10, List(5, 6, 30, 31, 32, 33, 34))
  }

  "d4s1" should {

    "solve the example" in {
      d4s1(
        List(
          "[1518-11-01 00:00] Guard #10 begins shift",
          "[1518-11-01 00:05] falls asleep",
          "[1518-11-01 00:25] wakes up",
          "[1518-11-01 00:30] falls asleep",
          "[1518-11-01 00:55] wakes up",
          "[1518-11-01 23:58] Guard #99 begins shift",
          "[1518-11-02 00:40] falls asleep",
          "[1518-11-02 00:50] wakes up",
          "[1518-11-03 00:05] Guard #10 begins shift",
          "[1518-11-03 00:24] falls asleep",
          "[1518-11-03 00:29] wakes up",
          "[1518-11-04 00:02] Guard #99 begins shift",
          "[1518-11-04 00:36] falls asleep",
          "[1518-11-04 00:46] wakes up",
          "[1518-11-05 00:03] Guard #99 begins shift",
          "[1518-11-05 00:45] falls asleep",
          "[1518-11-05 00:55] wakes up"
        )
      ) shouldBe 240
    }

    "solve" in {
      println(d4s1(PuzzleUtil.readInput(2018, 4)))
    }

  }

  "d4s2" should {

    "solve the example" in {
      d4s2(
        List(
          "[1518-11-01 00:00] Guard #10 begins shift",
          "[1518-11-01 00:05] falls asleep",
          "[1518-11-01 00:25] wakes up",
          "[1518-11-01 00:30] falls asleep",
          "[1518-11-01 00:55] wakes up",
          "[1518-11-01 23:58] Guard #99 begins shift",
          "[1518-11-02 00:40] falls asleep",
          "[1518-11-02 00:50] wakes up",
          "[1518-11-03 00:05] Guard #10 begins shift",
          "[1518-11-03 00:24] falls asleep",
          "[1518-11-03 00:29] wakes up",
          "[1518-11-04 00:02] Guard #99 begins shift",
          "[1518-11-04 00:36] falls asleep",
          "[1518-11-04 00:46] wakes up",
          "[1518-11-05 00:03] Guard #99 begins shift",
          "[1518-11-05 00:45] falls asleep",
          "[1518-11-05 00:55] wakes up"
        )
      ) shouldBe 4455
    }

    "solve" in {
      println(d4s2(PuzzleUtil.readInput(2018, 4)))
    }

  }

}
