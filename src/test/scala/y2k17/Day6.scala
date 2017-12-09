package y2k17

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec

class Day6 extends WordSpecLike with Matchers {

  def redistribute(banks: Seq[Int]): Seq[Int] = {
    val redistributableBlocks = banks.max
    val redistIndex = banks.indexOf(redistributableBlocks)
    val listWithoutRedistributable = banks.updated(redistIndex, 0)
    val easyDistribution =
      if (redistributableBlocks / banks.size > 0) {
        val evenlyAdd = redistributableBlocks / banks.size
        listWithoutRedistributable.map(_ + evenlyAdd)
      } else listWithoutRedistributable

    val reminderDistributable = redistributableBlocks % banks.size

    if (reminderDistributable > 0) {
      val distributeRight = if (redistIndex + reminderDistributable < banks.size) reminderDistributable else banks.size - 1 - redistIndex
      val distributeLeft = reminderDistributable - distributeRight

      //println(easyDistribution, distributeLeft, distributeRight, redistIndex)
      easyDistribution
        .patch(redistIndex + 1, easyDistribution.slice(redistIndex + 1, redistIndex + distributeRight + 1).map(_ + 1), distributeRight)
        .patch(0, easyDistribution.slice(0, distributeLeft).map(_ + 1), distributeLeft)
    } else {
      easyDistribution
    }
  }

  @tailrec
  final def run(list: Seq[Int], known: Set[Seq[Int]]): (Int, Seq[Int]) = {
    val newState = redistribute(list)
    if (list.size < newState.size) {
      println(list)
      throw new Exception()
    }
    val newSet = known + newState
    if (newSet.size == known.size) (known.size, newState)
    else run(newState, newSet)
  }

  def d6p1(list: Seq[Int]): Int = {
    run(list, Set(list))._1
  }

  def d6p2(list: Seq[Int]): Int = {
    val (steps, endList) = run(list, Set(list))
    val step2 = run(list, Set(list,endList))._1
    steps - step2 +1
  }

  "redistribution" should {

    val first = Seq(0, 2, 7, 0)
    val second = Seq(2, 4, 1, 2)
    val third = Seq(3, 1, 2, 3)
    val forth = Seq(0, 2, 3, 4)
    val fifth = Seq(1, 3, 4, 1)

    "work for the given examples 1" in {
      redistribute(first) shouldBe second
    }
    "work for the given examples 2" in {
      redistribute(second) shouldBe third
    }
    "work for the given examples 3" in {
      redistribute(third) shouldBe forth
    }
    "work for the given examples 4" in {
      redistribute(forth) shouldBe fifth
    }
    "work for the given examples 5" in {
      redistribute(fifth) shouldBe second
    }

    "works with problematic usecases too 1" in {
      val l = List(13, 1, 14, 12, 12, 9, 3, 7, 0, 6, 8, 4, 9, 2, 7, 5)
      redistribute(l) shouldBe Seq(14, 1, 0, 13, 13, 10, 4, 8, 1, 7, 9, 5, 10, 3, 8, 6)
    }

    "works with problematic usecases too 2" in {
      val l = List(13, 1, 0, 13, 13, 10, 4, 8, 1, 7, 9, 5, 10, 3, 8, 6)
      redistribute(l) shouldBe Seq(0, 2, 1, 14, 14, 11, 5, 9, 2, 8, 10, 6, 11, 4, 8, 6)
    }

  }

  "d6p1" should {
    "work for example" in {
      d6p1(Seq(0, 2, 7, 0)) shouldBe 5
    }

    "solve the puzzle" in {
      println(d6p1(Seq(14, 0, 15, 12, 11, 11, 3, 5, 1, 6, 8, 4, 9, 1, 8, 4)))
    }
  }

  "d6p2" should {
    "work for example" in {
      d6p2(Seq(0, 2, 7, 0)) shouldBe 4
    }

    "solve the puzzle" in {
      println(d6p2(Seq(14, 0, 15, 12, 11, 11, 3, 5, 1, 6, 8, 4, 9, 1, 8, 4)))
    }
  }

}
