package jobtests

import org.scalameter.{Bench, Gen}
import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec
import scala.util.{Random, Try}
import Jumper._

class Jumper extends WordSpecLike with Matchers {

  "Jumper" should {

    val methods = Seq(
      "frontToBack" -> frontToBack _,
      "backToFront" -> backToFront _
    )

    val cases = Seq(
      Seq(1) -> 1,
      Seq(0) -> 0,
      Seq(1, 1) -> 2,
      Seq(2, 0) -> 1,
      Seq(2, 0, 3, 4, 0, 0, 0) -> 3
    )

    methods.foreach { case (name, f) =>
      s"$name" should
        cases.foreach { case (in, out) =>
          s"$in should return $out" in {
            f(in.toList) shouldBe out
          }
        }
    }
  }

}

object JumperPerfTestF2B extends Bench.ForkedTime {
  val lists = for {
    size <- Gen.range("size")(5, 30, 5)
  } yield {
    val random = new Random(1)
    (1 to size).map(_ => random.nextInt(size/2))
  }

  measure method "frontToBack" in {
    using(lists) curve("Range") in { l =>
      frontToBack(l.toList)
    }
  }

  /*
  [info] Parameters(size -> 5): 0.082087 ms
  [info] Parameters(size -> 10): 0.026591 ms
  [info] Parameters(size -> 15): 1.356069 ms
  [info] Parameters(size -> 20): 49.349764 ms
  [info] Parameters(size -> 25): 22.964506 ms
  [info] Parameters(size -> 30): 0.001289 ms

  [info] Parameters(size -> 5): 0.01049 ms
  [info] Parameters(size -> 10): 0.007446 ms
  [info] Parameters(size -> 15): 0.043258 ms
  [info] Parameters(size -> 20): 1.206981 ms
  [info] Parameters(size -> 25): 14.561616 ms
  [info] Parameters(size -> 30): 127.731924 ms

   */
}

object JumperPerfTestB2F extends Bench.ForkedTime {
  val lists = for {
    size <- Gen.range("size")(500, 10000, 500)
  } yield {
    val random = new Random(1)
    (1 to size).map(_ => random.nextInt(size/2))
  }

  measure method "backToFront" in {
    using(lists) curve("Range") in { l =>
      backToFront(l.toList)
    }
  }
  /*
  [info] Parameters(size -> 100): 0.100244 ms
  [info] Parameters(size -> 200): 0.239254 ms
  [info] Parameters(size -> 300): 0.440802 ms
  [info] Parameters(size -> 400): 0.825741 ms
  [info] Parameters(size -> 500): 1.16052 ms
  [info] Parameters(size -> 600): 1.55233 ms
  [info] Parameters(size -> 700): 2.015228 ms
  [info] Parameters(size -> 800): 2.751522 ms
  [info] Parameters(size -> 900): 3.351713 ms
  [info] Parameters(size -> 1000): 4.179651 ms

  [info] Parameters(size -> 500): 0.816181 ms
  [info] Parameters(size -> 1000): 3.17508 ms
  [info] Parameters(size -> 1500): 6.890769 ms
  [info] Parameters(size -> 2000): 13.922883 ms
  [info] Parameters(size -> 2500): 21.810184 ms
  [info] Parameters(size -> 3000): 32.573491 ms
  [info] Parameters(size -> 3500): 46.85405 ms
  [info] Parameters(size -> 4000): 66.989528 ms
  [info] Parameters(size -> 4500): 86.195973 ms
  [info] Parameters(size -> 5000): 114.750706 ms
  [info] Parameters(size -> 5500): 145.840511 ms
  [info] Parameters(size -> 6000): 181.906274 ms
  [info] Parameters(size -> 6500): 222.770984 ms
  [info] Parameters(size -> 7000): 261.210306 ms
  [info] Parameters(size -> 7500): 306.593676 ms
  [info] Parameters(size -> 8000): 360.951904 ms
  [info] Parameters(size -> 8500): 351.277502 ms
  [info] Parameters(size -> 9000): 400.421468 ms
  [info] Parameters(size -> 9500): 447.224837 ms
  [info] Parameters(size -> 10000): 493.479602 ms

   */
}

object Jumper {
  def frontToBack(input: List[Int]): Int = {

    @tailrec
    def rec(in: (List[Int], Int, List[(Int, List[Int])], Int)): Int = {
      in match {
        case (actualList: List[Int], actualJumps: Int, leftovers: List[(Int, List[Int])], actualMin: Int) =>

          def deeperOrReturnWith(num: Int) = {
            val filteredLeftovers = if(num != actualMin && num != 0) leftovers.filter(_._1 < num) else leftovers
            filteredLeftovers match {
              case Nil => (Nil, 0, Nil, num)
              case (newJumps, newlist) :: newLeftovers =>
                (newlist, newJumps, newLeftovers, num)
            }
          }

          actualList match {
            case Nil if leftovers == Nil => actualMin
            case Nil => rec(deeperOrReturnWith(actualMin))
            case h :: _ if h == 0 => rec(deeperOrReturnWith(actualMin))
            case h :: tail if h > tail.size && actualMin == 0 => rec(deeperOrReturnWith(actualJumps + 1))
            case h :: tail if h > tail.size && actualJumps + 1 < actualMin => rec(deeperOrReturnWith(actualJumps + 1))
            case h :: tail if h > tail.size => rec(deeperOrReturnWith(actualMin))
            case h :: _ =>
              val tmpLeftovers = (1 to h).map(i => (actualJumps + 1) -> actualList.drop(i)).toList ++ leftovers
              val (newJumps, newlist) = tmpLeftovers.head
              val newLeftovers = tmpLeftovers.tail
              rec(newlist, newJumps, newLeftovers, actualMin)
          }
      }
    }

    rec(input, 0, List.empty, 0)
  }

  def backToFront(input: List[Int]): Int = {
    @tailrec
    def rec(listIBuild: List[Int], listIConsume: List[Int], i: Int): Int = {
      listIConsume match {
        case Nil => listIBuild.head
        case h :: tail if h == 0 => rec(0 :: listIBuild, tail, i+1)
        case h :: tail if h > i => rec(1 :: listIBuild, tail, i+1)
        case h :: tail =>
          val s = Try(listIBuild.take(h).filter(_ != 0).min).toOption.map(_ + 1).getOrElse(0)
          rec(s :: listIBuild, tail, i+1)
      }
    }

    rec(List.empty, input.reverse, 0)
  }
}
