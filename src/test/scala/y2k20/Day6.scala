package y2k20

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

class Day6 extends WordSpecLike with Matchers {


  def d6s1(in: Seq[String]) = {
    val blocks = in.foldLeft(List(List.empty[String])){case (acc, newL) => if(newL.trim.isEmpty) List.empty[String] :: acc else (newL :: acc.head) :: acc.tail }.reverse

    blocks.map(_.flatMap(_.toSeq).toSet.size).sum
  }

  def d6s2(in: Seq[String]) = {
    val blocks = in.foldLeft(List(List.empty[String])){case (acc, newL) => if(newL.trim.isEmpty) List.empty[String] :: acc else (newL :: acc.head) :: acc.tail }.reverse

    blocks.map(_.map(_.toSet).reduce(_.intersect(_)).size).sum
  }

  "d6s1" in {
    println(d6s1(PuzzleUtil.readInput(2020, 6)))
  }

  "d6s2" in {
    println(d6s2(PuzzleUtil.readInput(2020, 6)))
  }
}
