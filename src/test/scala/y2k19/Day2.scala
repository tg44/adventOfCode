package y2k19

import org.scalatest.{Matchers, WordSpecLike}

import scala.annotation.tailrec


class Day2 extends WordSpecLike with Matchers {
  //this task is a definition of the mutable faster than inmutable :(
  //but the puzzle size is not that big, the naive solution is under 2s

  def runProgram(l: Seq[Int]): Seq[Int] = {
    @tailrec
    def rec(acc: Seq[Int], pointer: Int): Seq[Int] = {
      if(pointer >= acc.length) {
        acc
      } else {
        val opcode = acc(pointer)
        if(opcode == 99) {
          acc
        } else {
          val read1 = acc(acc(pointer+1))
          val read2 = acc(acc(pointer+2))
          val out = acc(pointer+3)
          if(opcode == 1) {
            rec(acc.patch(out, Seq(read1+read2), 1), pointer+4)
          } else if(opcode == 2) {
            rec(acc.patch(out, Seq(read1*read2), 1), pointer+4)
          } else {
            throw new Exception()
          }
        }
      }
    }

    rec(l, 0)
  }


  Seq(
    Seq(1,0,0,0,99) -> Seq(2,0,0,0,99),
    Seq(2,3,0,3,99) -> Seq(2,3,0,6,99),
    Seq(2,4,4,5,99,0) -> Seq(2,4,4,5,99,9801),
    Seq(1,1,1,4,99,5,6,0,99) -> Seq(30,1,1,4,2,5,6,0,99),
    Seq(1,9,10,3,2,3,11,0,99,30,40,50) -> Seq(3500,9,10,70, 2,3,11,0, 99, 30,40,50)
  ).foreach{ case (in, out) =>
      s"ex: $in returns ok" in {
        runProgram(in) shouldBe out
      }
  }
  
  val input = Seq(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,10,19,23,1,6,23,27,1,5,27,31,1,10,31,35,2,10,35,39,1,39,5,43,2,43,6,47,2,9,47,51,1,51,5,55,1,5,55,59,2,10,59,63,1,5,63,67,1,67,10,71,2,6,71,75,2,6,75,79,1,5,79,83,2,6,83,87,2,13,87,91,1,91,6,95,2,13,95,99,1,99,5,103,2,103,10,107,1,9,107,111,1,111,6,115,1,115,2,119,1,119,10,0,99,2,14,0,0)
  "d2s1" in {
    val fixedInput = input.patch(1, Seq(12, 2), 2)
    println(runProgram(fixedInput).head)
  }

  def findNounAndVerb(req: Int) = {
    (for{
      i <- 0 to 99
      j <- 0 to 99
    } yield {
      (i, j)
    }).find{ case (n, v) =>
      val fixedInput = input.patch(1, Seq(n, v), 2)
      runProgram(fixedInput).head == req
    }
  }

  "d2s2" in {
    println(findNounAndVerb(19690720).map(r => 100* r._1 + r._2))
  }
}
