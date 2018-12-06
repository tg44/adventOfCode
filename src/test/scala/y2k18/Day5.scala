package y2k18

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

import scala.annotation.tailrec

class Day5 extends WordSpecLike with Matchers {

  def isOppositeCase(a: Char, b: Char): Boolean = a != b && a.toUpper == b.toUpper

  def reaction(poli: List[Char]): (Int, List[Char]) = {
    @tailrec
    def rec(c: Option[Char], rem: List[Char], acc: List[Char], noReactions: Int): (Int, List[Char]) = {
      c match {
        case Some(ch) =>
          rem match {
            case Nil => (noReactions, ch :: acc)
            case h :: t if isOppositeCase(ch, h) => rec(None, t, acc, noReactions+1)
            case h :: t => rec(Option(h), t, ch :: acc, noReactions)
          }
        case None =>
          rem match {
            case Nil => (noReactions, acc)
            case h :: t => rec(Option(h), t, acc, noReactions)
          }
      }
    }

    rec(None, poli.reverse, Nil, 0)
  }

  def d5s1(polimer: String) = {
    @tailrec
    def rec(in: List[Char]): List[Char] = {
      val (noReaction, out) = reaction(in)
      if(noReaction > 0) rec(out) else out
    }

    rec(polimer.toList).mkString
  }

  def d5s2(polimer: String) = {
    val reduced = d5s1(polimer)
    val newPoli = reduced.toList
    val charsInPoli = reduced.toUpperCase.toSet
    charsInPoli.map(c => newPoli.filter(pc => pc.toUpper != c)).map(p => d5s1(p.mkString)).map(_.size).min
  }

  "isOppositeCase" in {
    isOppositeCase('a', 'A') shouldBe true
    isOppositeCase('A', 'a') shouldBe true
    isOppositeCase('a', 'a') shouldBe false
    isOppositeCase('A', 'A') shouldBe false
    isOppositeCase('b', 'A') shouldBe false
    isOppositeCase('b', 'B') shouldBe true
  }

  "reaction" in {
    reaction("aA".toList) shouldBe (1, Nil)
    reaction("abAB".toList) shouldBe (0, "abAB".toList)
  }

  "d5s1" should {
    "solve example" in {
      d5s1("dabAcCaCBAcCcaDA") shouldBe "dabCBAcaDA"
    }

    "solve" in {
      println(d5s1(PuzzleUtil.readInput(2018, 5).head).size)
    }
  }

  "d5s2" should {
    "solve the example" in {
      d5s2("dabAcCaCBAcCcaDA") shouldBe 4
    }

    "solve" in {
      println(d5s2(PuzzleUtil.readInput(2018, 5).head))
    }
  }

}
