package y2k19

import org.scalatest.{Matchers, WordSpecLike}

class Day4 extends WordSpecLike with Matchers {

  def notDecreasing(l: Int): Boolean = {
    val digits = l.toString.map(_.toInt)
    digits.zip(digits.tail).forall{case (s, l) => s <= l}
  }

  def hasAdjacentDuplication(l: Int): Boolean = {
    val digits = l.toString.map(_.toInt)
    digits.zip(digits.tail).exists{case (s, l) => s == l}
  }

  def isValidPassword1(l: Int) = {
    notDecreasing(l) && hasAdjacentDuplication(l)
  }

  "notDecreasing" should {
    "ok" in {
      notDecreasing(123) shouldBe true
      notDecreasing(111) shouldBe true
      notDecreasing(110) shouldBe false
      notDecreasing(911) shouldBe false
    }
  }
  "hasAdjacentDuplication" should {
    "ok" in {
      hasAdjacentDuplication(123) shouldBe false
      hasAdjacentDuplication(111) shouldBe true
      hasAdjacentDuplication(110) shouldBe true
      hasAdjacentDuplication(911) shouldBe true
    }
  }

  "d4s1" in {
    println((359282 to 820401).count(isValidPassword1))
  }

  def hasDuplication(l: Int) = {
    val digits = l.toString.map(_.toInt)
    digits.groupBy(identity).mapValues(_.size).exists(_._2 == 2)
  }

  def isValidPassword2(l: Int) = {
    notDecreasing(l) && hasDuplication(l)
  }

  "d4s2" in {
    println((359282 to 820401).count(isValidPassword2))
  }
}
