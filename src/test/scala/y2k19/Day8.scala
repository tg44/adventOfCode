package y2k19

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

class Day8 extends WordSpecLike with Matchers {

  def splitToLayers(s: String, w: Int, h: Int) = {
    val elementsPerLayer = w*h
    s.grouped(elementsPerLayer)
  }

  "splitToLayers" in {
    splitToLayers("123456789012", 3, 2).size shouldBe 2
  }

  def d8s1(input: String) = {
    val l = splitToLayers(input, 25, 6).minBy(_.count(_ == '0'))
    l.count(_ == '1') * l.count(_ == '2')
  }

  "d8s1" should {
    "solve" in {
      val input = PuzzleUtil.readInput(2019, 8).head
      println(d8s1(input))
    }
  }

  def d8s2(input: String, w: Int = 25, h: Int = 6): String = {
    val layers = splitToLayers(input, w, h)

    def addLayers(l1: String, l2: String): String = {
      l1.zip(l2).map{ case (a, b) =>
        if(a != '2') a else b
      }
    }.mkString

    layers.reduce(addLayers)
  }

  "d8s2" should {
    "example" in {
      val input = "0222112222120000"
      d8s2(input, 2, 2) shouldBe "0110"
    }
    "solve" in {
      val input = PuzzleUtil.readInput(2019, 8).head
      println(d8s2(input).map(x => if(x == '0') ' ' else '#').grouped(25).mkString("\n"))
    }
  }

}
