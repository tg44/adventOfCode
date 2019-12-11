package y2k19

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec

class Day10 extends WordSpecLike with Matchers {

  def parseMap(l: Seq[String]): Seq[(Int, Int)] = {
    l.map(_.zipWithIndex).zipWithIndex
      .flatMap{case (inner, y) => inner.map{case (char, x) => char -> (x, y)}}
      .collect{case ('#', coords) => coords}
  }

  "parseMap" in {
    val input = Seq(
      ".#..#",
      ".....",
      "#####",
      "....#",
      "...##",
    )
    parseMap(input) shouldBe Seq(
      1 -> 0, 4 -> 0,
      0 -> 2, 1 -> 2, 2 -> 2, 3 -> 2, 4 -> 2,
      4 -> 3,
      3 -> 4, 4 -> 4
    )
  }

  def factorize(x: Int): List[Int] = {
    @tailrec
    def foo(x: Int, a: Int = 2, list: List[Int] = Nil): List[Int] = a*a > x match {
      case false if x % a == 0 => foo(x / a, a    , a :: list)
      case false               => foo(x    , a + 1, list)
      case true                => x :: list
    }
    foo(x)
  }

  def getCommonElements(l1: Seq[Int], l2: Seq[Int]): Seq[Int] = {
    @tailrec
    def rec(acc: List[Int], l1: List[Int], l2: List[Int]): List[Int] = {
      l1 match {
        case Nil => acc
        case h1 :: t1 =>
          l2 match {
            case Nil => acc
            case h2 :: t2 if h1 == h2 =>
              rec(h1 :: acc, t1, t2)
            case h2 :: t2 if h1 > h2 =>
              rec(acc, l1, t2)
            case _ =>
              rec(acc, t1, l2)
          }
      }
    }
    rec(List.empty, l1.sorted.toList, l2.sorted.toList)
  }

  def seeCount(from: (Int, Int), all: Seq[(Int, Int)]): Int = {
    val notMe = all.filter(_ != from)
    val vectors = notMe.map(v => v -> ( (v._1 - from._1) -> (v._2 - from._2)))
    val normalizedToWhole = vectors.map{ case (v, (x, y)) =>
        if(x == 0){
          v -> (0,y/Math.abs(y))
        } else if (y == 0) {
          v -> (x/Math.abs(x),0)
        } else {
            val xF = factorize(Math.abs(x))
            val yF = factorize(Math.abs(y))
            val divisior = getCommonElements(xF, yF).product
            v -> (x / divisior, y / divisior)
        }
    }
    normalizedToWhole.map(_._2).toSet.size
  }

  def d10s1(map: Seq[String]) = {
    val asteroids = parseMap(map)
    val seeMap = asteroids.map(a => a -> seeCount(a, asteroids))
    seeMap.maxBy(_._2)
  }

  "d10s1" should {
    "example1" in {
      val input = Seq(
        ".#..#",
        ".....",
        "#####",
        "....#",
        "...##",
      )
      d10s1(input) shouldBe ((3, 4), 8)
    }
    "example2" in {
      val input = Seq(
        "......#.#.",
        "#..#.#....",
        "..#######.",
        ".#.#.###..",
        ".#..#.....",
        "..#....#.#",
        "#..#....#.",
        ".##.#..###",
        "##...#..#.",
        ".#....####",
      )
      d10s1(input) shouldBe ((5, 8), 33)
    }
    "example3" in {
      val input = Seq(
        "#.#...#.#.",
        ".###....#.",
        ".#....#...",
        "##.#.#.#.#",
        "....#.#.#.",
        ".##..###.#",
        "..#...##..",
        "..##....##",
        "......#...",
        ".####.###.",
      )
      d10s1(input) shouldBe ((1, 2), 35)
    }
    "solve" in {
      val input = Seq(
        "#..#.#.###.#...##.##....",
        ".#.#####.#.#.##.....##.#",
        "##..#.###..###..#####..#",
        "####.#.#..#....#..##.##.",
        ".#######.#####...#.###..",
        ".##...#.#.###..###.#.#.#",
        ".######.....#.###..#....",
        ".##..##.#..#####...###.#",
        "#######.#..#####..#.#.#.",
        ".###.###...##.##....##.#",
        "##.###.##.#.#..####.....",
        "#.#..##..#..#.#..#####.#",
        "#####.##.#.#.#.#.#.#..##",
        "#...##.##.###.##.#.###..",
        "####.##.#.#.####.#####.#",
        ".#..##...##..##..#.#.##.",
        "###...####.###.#.###.#.#",
        "..####.#####..#####.#.##",
        "..###..###..#..##...#.#.",
        "##.####...##....####.##.",
        "####..#..##.#.#....#..#.",
        ".#..........#..#.#.####.",
        "###..###.###.#.#.#....##",
        "########.#######.#.##.##",
      )
      println(d10s1(input))
    }
  }

}
