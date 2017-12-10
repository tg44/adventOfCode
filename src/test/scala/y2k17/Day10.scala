package y2k17

import org.scalatest.{Matchers, WordSpecLike}

class Day10 extends WordSpecLike with Matchers {

  def knot(list: List[Int], listOffset: Int, skipSize: Int, length: Int): (List[Int], Int, Int) = {
    //println(list)
    val knotedList = list.patch(0,list.slice(0,length).reverse,length)
    //println(knotedList)
    val jumpLength = (length + skipSize) % list.size
    val (s1, s2) = knotedList.splitAt(jumpLength)
    val jumpedList = s2 ::: s1
    //println(jumpedList)
    (jumpedList, listOffset+jumpLength, skipSize+1)
  }

  def knotSequence(lengthList: List[Int])(list: List[Int], listOffset: Int =0, skipSize: Int =0): List[Int] = {
    lengthList match {
      case Nil =>
        val (s1, s2) = list.splitAt(list.size -(listOffset % list.size))
        s2 ::: s1
      case head::tail => (knotSequence(tail) _).tupled(knot(list, listOffset, skipSize, head))
    }
  }

  def knotSequence2(lengthList: List[Int])(list: List[Int], listOffset: Int =0, skipSize: Int =0): (List[Int], Int, Int) = {
    lengthList match {
      case Nil => (list, listOffset, skipSize)
      case head::tail => (knotSequence2(tail) _).tupled(knot(list, listOffset, skipSize, head))
    }
  }

  def iterateKnots(lengthList: List[Int], iterationsLeft: Int)(list: List[Int], listOffset: Int =0, skipSize: Int =0): List[Int] = {
    if(iterationsLeft > 0) {
      (iterateKnots(lengthList, iterationsLeft-1) _).tupled(knotSequence2(lengthList)(list, listOffset, skipSize))
    } else {
      val (s1, s2) = list.splitAt(list.size -(listOffset % list.size))
      s2 ::: s1
    }
  }

  def d10p1(lengthList: List[Int]): Int = {
    val hashed = knotSequence(lengthList)((0 to 255).toList)
    hashed.head * hashed.tail.head
  }

  def d10p2(input: String): String = {
    val initialList = (0 to 255).toList
    val lengthList = input.map(_.toInt).toList ::: List(17, 31, 73, 47, 23)
    //println(lengthList)
    val spareHash = iterateKnots(lengthList, 64)(initialList)
    //println(spareHash)
    val denseHash = spareHash.grouped(16).toList.map(_.reduce(_ ^ _))
    //println(denseHash)
    denseHash.map(_.toHexString).map(s => if (s.length < 2) "0"+s else s).mkString
  }

  "knotSequence" should {

    "work for the first" in {
      knotSequence(List(3))(List(0, 1, 2, 3, 4)) shouldBe Seq(2, 1, 0, 3, 4)
    }

    "work for the second" in {
      knotSequence(List(4))(List(3, 4, 2, 1, 0), 3, 1) shouldBe Seq(4, 3, 0, 1, 2)
    }

    "work for the whole" in {
      knotSequence(List(3, 4, 1, 5))(List(0, 1, 2, 3, 4)) shouldBe Seq(3, 4, 2, 1, 0)
    }

  }

  "d10p2" should {
    val tests = Seq(
      ("", "a2582a3a0e66e6e86e3812dcb672a272"),
      ("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd"),
      ("1,2,3", "3efbe78a8d82f29979031a4aa0b16a9d"),
      ("1,2,4", "63960835bcdc130f0b66d7ff4f6a5a8e")
    )

    tests.foreach {
      case (input, expected) =>
        s"return $expected for $input" in {
          d10p2(input) shouldBe expected
        }
    }
  }

  "d10p2 miscs" should {
    "xor" in {
      Seq(65 , 27 , 9 , 1 , 4 , 3 , 40 , 50 , 91 , 7 , 6 , 0 , 2 , 5 , 68 , 22).reduce(_ ^ _) shouldBe 64
    }

    "toHex" in {
      Seq(64, 7, 255).map(_.toHexString).map(s => if (s.length < 2) "0"+s else s).mkString shouldBe "4007ff"
    }

    "empty" in {
      d10p2("") shouldBe "a2582a3a0e66e6e86e3812dcb672a272"
    }
  }

  "Day10" should {

    "part1 works for the given input" in {
      println(d10p1(List(189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62)))
    }
    "part2 works for the given input" in {
      println(d10p2("189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62"))
    }

  }

}
