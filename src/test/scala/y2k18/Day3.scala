package y2k18

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

import scala.annotation.tailrec

class Day3 extends WordSpecLike with Matchers {

  case class Rect(id: Int, x: Int, y: Int, w: Int, h: Int)

  object Rect {
    def apply(str: String): Rect = {
      //#1 @ 1,3: 4x4
      val tmp1 = str.split("@")
      val idPart = tmp1(0).trim.tail
      val tmp2 = tmp1(1).split(":")
      val coordinatePart = tmp2(0).trim.split(",")
      val whPart = tmp2(1).trim.split("x")
      Rect(idPart.toInt, coordinatePart(0).toInt, coordinatePart(1).toInt, whPart(0).toInt, whPart(1).toInt)
    }
  }

  def getCoveredCoordinates(rect: Rect) = {
    for {
      i <- rect.x until (rect.x + rect.w)
      j <- rect.y until (rect.y + rect.h)
    } yield (i, j)
  }

  def d3s1(rects: List[Rect]) = {
    rects.flatMap(getCoveredCoordinates).groupBy(identity).mapValues(_.size).count { case ((_, _), c) => c > 1 }
  }

  def d3s2(rects: List[Rect]) = {
    val fieldsWithCoords = rects.map(r => (r.id,getCoveredCoordinates(r)))
    val goodCoords = fieldsWithCoords.flatMap(_._2).groupBy(identity).mapValues(_.size).filter { case ((_, _), c) => c == 1 }.keys.toSet
    fieldsWithCoords.find{case (id, coords) => coords.forall(goodCoords.contains)}.get._1
  }


  "can parse Rect" in {
    Rect("#1 @ 1,3: 4x4") shouldBe Rect(1, 1, 3, 4, 4)
  }

  "cover compute is valid" in {
    getCoveredCoordinates(Rect(1, 1, 2, 1, 2)) shouldBe Seq((1, 2), (1, 3))
  }

  "d3s1" should {
    "work on the example" in {
      d3s1(List("#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2").map(Rect(_))) shouldBe 4
    }
    "solves the puzzle" in {
      println(d3s1(PuzzleUtil.readInput(2018,3).map(Rect(_))))
    }
  }

  "d3s2" should {
    "work on the example" in {
      d3s2(List("#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2").map(Rect(_))) shouldBe 3
    }
    "solves the puzzle" in {
      println(d3s2(PuzzleUtil.readInput(2018,3).map(Rect(_))))
    }
  }

}
