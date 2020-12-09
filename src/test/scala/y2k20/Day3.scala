package y2k20

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

class Day3 extends WordSpecLike with Matchers {



  def originalInput =
    """..##.......
      |#...#...#..
      |.#....#..#.
      |..#.#...#.#
      |.#...##..#.
      |..#.##.....
      |.#.#.#....#
      |.#........#
      |#.##...#...
      |#...##....#
      |.#..#...#.#""".stripMargin.lines.toSeq


  def countTrees(lines: Seq[String], step: (Int, Int)): Long = {
    import cats.implicits._
    val startCoord = (0,0)

    def rec(current: (Int,Int), acc: Int): Long = {
      val next = current |+| step
      if(next._2 >= lines.size) {
        acc
      } else {
        val l = lines(next._2)
        val rightSteps = next._1 % l.length
        if(l(rightSteps) == '#') rec(next, acc+1)
        else rec(next, acc)
      }
    }
    rec(startCoord, 0)
  }

  "d3s1" should {
    "complete the example" in {
      countTrees(originalInput, (3, 1)) shouldBe 7
    }

    "complete" in {
      println(countTrees(PuzzleUtil.readInput(2020, 3), (3, 1)))
    }
  }
  "d3s2" should {
    "complete the example" in {
      countTrees(originalInput, (1, 1)) shouldBe 2
      countTrees(originalInput, (3, 1)) shouldBe 7
      countTrees(originalInput, (5, 1)) shouldBe 3
      countTrees(originalInput, (7, 1)) shouldBe 4
      countTrees(originalInput, (1, 2)) shouldBe 2
    }

    "complete" in {
      val res =
      countTrees(PuzzleUtil.readInput(2020, 3), (1, 1)) *
      countTrees(PuzzleUtil.readInput(2020, 3), (3, 1)) *
      countTrees(PuzzleUtil.readInput(2020, 3), (5, 1)) *
      countTrees(PuzzleUtil.readInput(2020, 3), (7, 1)) *
      countTrees(PuzzleUtil.readInput(2020, 3), (1, 2))
      println(res)
    }
  }



  /*
  *
  *
  *
  *
  * */
  def countTrees2(lines: Seq[String], right: Int, down: Int): Long = {
    val linesToVisit = lines.grouped(down).map(_.mkString).toSeq
    val treeCount    = for {
      i <- linesToVisit.indices
    } yield {
      isTree(i, linesToVisit(i), right, down)
    }

    treeCount.sum
  }

  def isTree(lineNr: Int, line: String, right: Int, down: Int): Int = {
    val split              = line.split("(?<=\\G.)")
    val characterNrToCheck = (lineNr * right) % split.length
    val characterToCheck   = split(characterNrToCheck)
    if (characterToCheck == "#") {
      1
    } else {
      0
    }
  }

  "complete the example" in {
    countTrees2(originalInput, 1, 1) shouldBe 2
    countTrees2(originalInput, 3, 1) shouldBe 7
    countTrees2(originalInput, 5, 1) shouldBe 3
    countTrees2(originalInput, 7, 1) shouldBe 4
    countTrees2(originalInput, 1, 2) shouldBe 2
  }
}
