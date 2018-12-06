package y2k18

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

import scala.annotation.tailrec

class Day2 extends WordSpecLike with Matchers {

  implicit class BooleanToInt(b: Boolean) {
    def toInt = if (b) 1 else 0
  }

  implicit class HammingDistance4Strings(str: String) {
    def hammingDistance(other: String) = {
      str.toList.zip(other.toList).count { case (left, right) => left != right }
    }
  }

  def d2s1(input: List[String]) = {
    val (no2s, no3s) = input.map { str =>
      val lettercounts = str.toList.groupBy(identity).map(_._2.size)
      (lettercounts.exists(_ == 2), lettercounts.exists(_ == 3))
    }
      .foldLeft((0, 0))((acc, data) => (acc._1 + data._1.toInt, acc._2 + data._2.toInt))
    no2s * no3s
  }

  def d2s2(input: List[String]): String = {
    @tailrec
    def findDistance1Strs(shrinkingList: List[String]): (String, String) = {
      shrinkingList match {
        case h :: t =>
          t.find(_.hammingDistance(h) == 1) match {
            case None => findDistance1Strs(t)
            case Some(s) => (h, s)
          }
        case _ => ("", "")
      }
    }

    val (s1, s2) = findDistance1Strs(input)
    s1.toList.zip(s2.toList).filter{ case (left, right) => left == right }.map(_._1).mkString
  }

  "d2s1" should {
    "solve the example" in {
      val example = List(
        "abcdef",
        "bababc",
        "abbcde",
        "abcccd",
        "aabcdd",
        "abcdee",
        "ababab"
      )
      d2s1(example) shouldBe 12
    }

    "solve the task" in {
      println(d2s1(PuzzleUtil.readInput(2018, 2)))
    }
  }

  "d2s2" should {
    "solve the example" in {
      val example = List(
        "abcde",
        "fghij",
        "klmno",
        "pqrst",
        "fguij",
        "axcye",
        "wvxyz"
      )
      d2s2(example) shouldBe "fgij"
    }
    "solve the task" in {
      println(d2s2(PuzzleUtil.readInput(2018, 2)))
    }
  }
}

