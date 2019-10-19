package jobtests

import org.scalatest.{Matchers, WordSpecLike}

class Jumper extends WordSpecLike with Matchers {

  "Jumper" should {

    val cases = Seq(
      Seq(1) -> 1,
      Seq(0) -> 0,
      Seq(1, 1) -> 2,
      Seq(2, 0) -> 1,
      Seq(2, 0, 3, 4, 0, 0, 0) -> 3
    )

    cases.foreach{ case (in, out) =>
      s"$in should return $out" in {
        solve(in) shouldBe out
      }
    }

  }

  def solve(input: Seq[Int]): Int = ???

}
