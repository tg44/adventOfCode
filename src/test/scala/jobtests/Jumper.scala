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

  def solve(input: Seq[Int]): Int = frontToBack(input.toList)

  def frontToBack(input: List[Int]): Int = {

    def rec(actualList: List[Int], actualJumps: Int, leftovers: List[(Int, List[Int])], actualMin: Int): Int = {
      def deeperOrReturnWith(num: Int) = {
        leftovers match {
          case Nil => num
          case (newJumps, newlist) :: newLeftovers =>
            rec(newlist, newJumps, newLeftovers, num)
        }
      }
      actualList match {
        case Nil =>  deeperOrReturnWith(actualMin)
        case h :: _ if h == 0 => deeperOrReturnWith(actualMin)
        case h :: tail if h > tail.size && actualMin == 0 => deeperOrReturnWith(actualJumps + 1)
        case h :: tail if h > tail.size && actualJumps + 1 < actualMin =>  deeperOrReturnWith(actualJumps + 1)
        case h :: tail if h > tail.size =>  deeperOrReturnWith(actualMin)
        case h :: _ =>
          val tmpLeftovers = (1 to h).map(i => (actualJumps + 1) -> actualList.drop(i)).toList ++ leftovers
          val (newJumps, newlist) = tmpLeftovers.head
          val newLeftovers = tmpLeftovers.tail
          println(s"$actualJumps -> $newLeftovers")
          rec(newlist, newJumps, newLeftovers, actualMin)
      }
    }

    rec(input, 0, List.empty, 0)
  }
}
