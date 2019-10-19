package jobtests

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec
import scala.util.Try

class Jumper extends WordSpecLike with Matchers {

  "Jumper" should {

    val cases = Seq(
      Seq(1) -> 1,
      Seq(0) -> 0,
      Seq(1, 1) -> 2,
      Seq(2, 0) -> 1,
      Seq(2, 0, 3, 4, 0, 0, 0) -> 3
    )

    cases.foreach{ case (in, out) =>
      s"$in should return $out" in {
        solve(in) shouldBe out
      }
    }

  }

  def solve(input: Seq[Int]): Int = backToFront(input.toList)

  def frontToBack(input: List[Int]): Int = {

    @tailrec
    def rec(in: (List[Int], Int, List[(Int, List[Int])], Int)): Int = {
      in match {
        case (actualList: List[Int], actualJumps: Int, leftovers: List[(Int, List[Int])], actualMin: Int) =>

          def deeperOrReturnWith(num: Int) = {
            leftovers match {
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
              println(s"$actualJumps -> $newLeftovers")
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
