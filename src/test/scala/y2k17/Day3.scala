package y2k17

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec
import scala.collection.immutable

class Day3 extends WordSpecLike with Matchers {

  def quad(x: Long): Long = x*x

  def smallestQuadOfOddAboveX(g: Long): Long = {
    @tailrec
    def helper(h: Long): Long ={
      if(quad(h+2)>=g) h+2 else helper(h+2)
    }

    helper(1)
  }

  def d3p1(x: Long): Long = {
    //bcs of mathematics and stuff
    val f = smallestQuadOfOddAboveX(x)
    val minimalDistance = f / 2
    val maxDistance = minimalDistance * 2

    val qf = quad(f)
    val s = qf - x

    val k = s % maxDistance
    maxDistance - k
  }
  case class Coordinate(x: Long, y: Long) {
    def getNeighbours: Seq[Coordinate] = {
      val all = for{
        i <- x-1 to x+1
        j <- y-1 to y+1
      } yield Coordinate(i,j)
      all.filter(_ != this)
    }
  }

  val coordinateStream = Stream.cons(Coordinate(0,0), getNthTailCoord(1))

  def getNthTailCoord(n: Int): Stream[Coordinate] = {
    Stream.cons( getNthCoordinate(n), getNthTailCoord(n+1))
  }

  def getNthCoordinate(n: Int): Coordinate = {
    //bcs of mathematics and stuff
    val np1 = n+1
    val f = smallestQuadOfOddAboveX(np1)
    val fm1 = f-1
    val k = f / 2
    val z = np1-quad(f-2)
    if(quad(f) == np1) {
      Coordinate(k, -1 * k)
    } else if( z < fm1) {
      Coordinate(k, -1* k + z)
    } else if(z < 2*fm1) {
      Coordinate(k-z+fm1, k)
    } else if(z < 3*fm1) {
      Coordinate(-1*k, k-z+2*fm1)
    } else {
      Coordinate(-1*k+z-3*fm1, -1*k)
    }
  }

  val stream: Stream[Long] = Stream.cons(1, getNthTail(1))

  def getNthTail(n: Int): Stream[Long] = {
    Stream.cons(getNthElement(n) , getNthTail(n+1))
  }

  def getNthElement(n: Int): Long = {
    coordinateStream(n)
      .getNeighbours
      .map(nb => coordinateStream.indexOf(nb))
      .filter(_ < n)
      .map(stream(_))
      .sum
  }

  def d3p2(x: Int): Long = {
    stream.find(_ > x).get
  }



  "smallestQuadOfOddAboveX" should {

    "return 3 for 9" in {
      smallestQuadOfOddAboveX(9) shouldBe 3
    }

    "return 3 for 6" in {
      smallestQuadOfOddAboveX(6) shouldBe 3
    }

    "return 5 for 16" in {
      smallestQuadOfOddAboveX(16) shouldBe 5
    }

    "return 7 for 26" in {
      smallestQuadOfOddAboveX(26) shouldBe 7
    }

  }

  "d3p1" should {

    val tests = Seq((2, 1), (6,1), (3,2), (12, 3), (13, 4), (23, 2), (28, 3), (1024, 31), (17, 4))

    tests.foreach{
      case (input, expected) =>
        s"return $expected for $input" in {
          d3p1(input) shouldBe expected
        }
    }

    "solve the given input" in {
      println(d3p1(289326))
    }

  }

  "getNthCoordinate" should {

    val tests = Seq(
      (1, Coordinate(0,0)),
      (11, Coordinate(2,0)),
      (12, Coordinate(2,1)),
      (13, Coordinate(2,2)),
      (14, Coordinate(1,2)),
      (15, Coordinate(0,2)),
      (17, Coordinate(-2,2)),
      (20, Coordinate(-2,-1)),
      (22, Coordinate(-1,-2)),
      (25, Coordinate(2,-2))
    )

    tests.foreach{
      case (input, expected) =>
        s"return $expected for $input" in {
          coordinateStream(input-1) shouldBe expected
        }
    }

  }

  "Coordinates" should {
    "getneighbours returns 8 element" in {
      Coordinate(0,0).getNeighbours.size shouldBe 8
    }
  }

  "stream" should {
    val tests = Seq(
      (1, 1),
      (2,1),
      (4, 4),
      (5, 5),
      (6, 10),
      (11, 54),
      (14, 122)
    )
    tests.foreach{
      case (input, expected) =>
        s"return $expected for $input" in {
          stream(input-1) shouldBe expected
        }
    }
  }

  "d3p2" should {
    val tests = Seq(
      (1, 2),
      (2, 4),
      (4, 5),
      (5, 10),
      (11, 23),
      (748, 806)
    )
    tests.foreach{
      case (input, expected) =>
        s"return $expected for $input" in {
          d3p2(input) shouldBe expected
        }
    }

    "solve the given input" in {
      println(d3p2(289326))
    }
  }

}
