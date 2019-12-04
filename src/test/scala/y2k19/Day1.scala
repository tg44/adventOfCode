package y2k19

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

import scala.annotation.tailrec

class Day1 extends WordSpecLike with Matchers {

  def mass(i: Long): Long = {
    i/3-2
  }

  def d1s1(l: Seq[Long]): Long = {
    l.foldLeft(0L)(_ + mass(_))
  }

  "mass" should {
    Seq(
      12 -> 2,
      14 -> 2,
      1969 -> 654,
      100756 -> 33583
    ).foreach{ case (in, out) =>
      s"for $in return $out" in {
        mass(in) shouldBe out
      }
    }
  }

  "d1s1" should {
    "solve" in {
      println(d1s1(PuzzleUtil.readInput(2019, 1).map(_.toLong)))
    }
  }

  def fuelOfFuel(i: Long): Long = {
    @tailrec
    def rec(acc: Long, act: Long): Long = {
      val m = mass(act)
      if(m<=0) acc
      else rec(m+acc, m)
    }
    rec(0, i)
  }

  "fuelOfFuel" should {
    Seq(
      33583 -> 50346,
      654 -> 966,
      14 -> (14+2)
    ).foreach{ case (in, out) =>
      s"for $in return ${out-in}" in {
        fuelOfFuel(in) shouldBe (out-in)
      }
    }
  }

  def d1s2(l: Seq[Long]): Long = {
    l.foldLeft(0L){case (acc, i) =>
      val f = mass(i)
      val fof = fuelOfFuel(f)
      acc + f + fof
    }
  }

  "d1s2" should {
    "solve" in {
      val input = PuzzleUtil.readInput(2019, 1).map(_.toLong)
      val fuel = d1s1(input)
      val fof = fuelOfFuel(fuel)
      println(fuel+fof)
      println(d1s2(input))
    }
  }
}
