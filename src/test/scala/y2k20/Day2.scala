package y2k20

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

class Day2 extends WordSpecLike with Matchers {

  def checkValidityWithPolicy(in: String) = {
    val s = in.split(":")
    val rule = s(0)
    val pwd = s(1).trim
    val s2 = rule.split(" ")
    val s3 = s2(0).split("-")
    val min = s3(0).trim.toInt
    val max = s3(1).trim.toInt
    val char = s2(1).takeWhile(_ != ':').trim.head
    val count = pwd.count(_ == char)
    min <= count && count <= max
  }

  def checkValidityWithSecondPolicy(in: String) = {
    val s = in.split(":")
    val rule = s(0)
    val pwd = s(1).trim
    val s2 = rule.split(" ")
    val s3 = s2(0).split("-")
    val first = s3(0).trim.toInt
    val second = s3(1).trim.toInt
    val char = s2(1).takeWhile(_ != ':').trim.head
    (pwd(first-1) == char) ^ (pwd(second-1) == char)
  }

  "d2s1" should {
    "solve examples" in {
      checkValidityWithPolicy("1-3 a: abcde") shouldBe true
      checkValidityWithPolicy("1-3 b: cdefg") shouldBe false
      checkValidityWithPolicy("2-9 c: ccccccccc") shouldBe true
    }

    "solve" in {
      println(PuzzleUtil.readInput(2020, 2).map(checkValidityWithPolicy).count(identity))
    }

  }

  "d2s2" should {
    "solve examples" in {
      checkValidityWithSecondPolicy("1-3 a: abcde") shouldBe true
      checkValidityWithSecondPolicy("1-3 b: cdefg") shouldBe false
      checkValidityWithSecondPolicy("2-9 c: ccccccccc") shouldBe false
    }

    "solve" in {
      println(PuzzleUtil.readInput(2020, 2).map(checkValidityWithSecondPolicy).count(identity))
    }

  }

}
