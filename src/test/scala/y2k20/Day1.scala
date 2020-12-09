package y2k20

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

class Day1 extends WordSpecLike with Matchers {

  def d1s1(in: Seq[String]): Long = {
    in.map(_.toLong).combinations(2).find(_.sum == 2020).map(_.product).getOrElse(0)
  }

  def d1s2(in: Seq[String]): Long = {
    in.map(_.toLong).combinations(3).find(_.sum == 2020).map(_.product).getOrElse(0)
  }

  "d1s1" should {
    "solve the example" in {
      val in = """1721
                 |979
                 |366
                 |299
                 |675
                 |1456""".stripMargin
      d1s1(in.lines.toSeq) shouldBe 514579
    }
    "solve" in {
      println(d1s1(PuzzleUtil.readInput(2020, 1)))
    }
  }

  "d1s2" should {
    "solve the example" in {
      val in = """1721
                 |979
                 |366
                 |299
                 |675
                 |1456""".stripMargin
      d1s2(in.lines.toSeq) shouldBe 241861950
    }
    "solve" in {
      println(d1s2(PuzzleUtil.readInput(2020, 1)))
    }
  }

}
