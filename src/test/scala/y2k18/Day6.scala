package y2k18

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

import scala.annotation.tailrec

class Day6 extends WordSpecLike with Matchers {

  case class Coord(x: Int, y: Int)
  object Coord {
    def apply(s: String): Coord = {
      val c = s.split(",").map(_.trim.toInt)
      Coord(c.head, c.last)
    }
  }

  def manhattan(coord: Coord, distance: Int) = {
    (for{
      i <- -distance to distance
      absI = if(i<0) -1*i else i
      j <- Seq(-distance + absI, distance - absI)
    } yield Coord(i+ coord.x,j + coord.y)).toSet
  }

  def distance(coord1: Coord, coord2: Coord) = {
    val x = coord1.x - coord2.x
    val y = coord1.y - coord2.y
    val xabs = if(x < 0) -x else x
    val yabs = if(y < 0) -y else y
    xabs + yabs
  }

  def d6s1(coords: List[Coord]) = {
    val rectP1 = Coord(coords.minBy(_.x).x, coords.minBy(_.y).y)
    val rectP2 = Coord(coords.maxBy(_.x).x, coords.maxBy(_.y).y)
    val r = (rectP2.x-rectP1.x + rectP2.y-rectP1.y)/2
    println(r)
    @tailrec
    def rec(n: Int, state: Map[Coord, (Option[Coord], Int)]): Map[Coord, (Option[Coord], Int)] = {
      if (n > r) {
        state
      } else {
        val newPoints: Map[Coord, Option[Coord]] = coords.flatMap(c => manhattan(c, n).map(_ -> c)).groupBy(_._1).mapValues(l => if(l.size == 1) l.headOption.map(_._2) else None)
        val filtered = newPoints.filterKeys(state.get(_).isEmpty)
        val newState = filtered.mapValues(_ -> n) ++ state
        rec(n+1, newState)
      }
    }
    val finalState = rec(0, Map.empty)
    val infinitePoints = finalState.filter(_._2._2 == r).map(_._2._1).toSet + None
    val removeInfinites = finalState.toList.map(s => s._2._1 -> s._1).groupBy(_._1).filterKeys(!infinitePoints.contains(_)).mapValues(_.size)
    removeInfinites.maxBy(_._2)._2
  }

  def d6s2(coords: List[Coord], range: Int) = {
    val rect = for {
      i <- coords.minBy(_.x).x to coords.maxBy(_.x).x
      j <- coords.minBy(_.y).y to coords.maxBy(_.y).y
    } yield Coord(i,j)

    val distances = rect.map(r => (r, coords.map(distance(_, r)).sum))
    distances.count(_._2 < range)
  }

  "d6s1" should {
    "solve example" in {
      d6s1(List(
      "1, 1",
      "1, 6",
      "8, 3",
      "3, 4",
      "5, 5",
      "8, 9"
      ).map(Coord(_))) shouldBe 17
    }

    "solve" in {
      println(d6s1(PuzzleUtil.readInput(2018, 6).map(Coord(_))))
    }
  }

  "d6s2" should {
    "solve example" in {
      d6s2(List(
        "1, 1",
        "1, 6",
        "8, 3",
        "3, 4",
        "5, 5",
        "8, 9"
      ).map(Coord(_)), 32) shouldBe 16
    }

    "solve" in {
      println(d6s2(PuzzleUtil.readInput(2018, 6).map(Coord(_)), 10000))
    }
  }

}
