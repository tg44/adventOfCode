package y2k18

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

//first implementation with immutable.List was really slow, solved s1 in 2 minutes so this code is not PF :(
// with listBuffer s1 solved in 23s
// naive cursor solution 658ms
class Day9Naive extends WordSpecLike with Matchers {

  class Node(val value: Int){
    private var prev: Node = this
    private var next: Node = this

    def navigateRight(x: Int): Node = {
      if(x == 0) this
      else next.navigateRight(x-1)
    }

    def navigateLeft(x: Int): Node = {
      if(x == 0) this
      else prev.navigateLeft(x-1)
    }

    def addNewNodeAfterThis(value: Int): Node = {
      val newNode = new Node(value)
      newNode.prev = this
      newNode.next = next
      next.prev = newNode
      this.next = newNode
      next
    }

    def deleteThisNode(): Node = {
      prev.next = next
      next.prev = prev
      next
    }

    def toList = {
      @tailrec
      def rec(starting: Node, cursor: Node, acc: List[Int]): List[Int] = {
        if(cursor.next == starting) {
          (cursor.value :: acc).reverse
        } else {
          rec(starting, cursor.next, cursor.value :: acc)
        }
      }
      rec(this, this, List.empty)
    }
  }

  def d9s1(players: Int, marbles: Int) = {
    val rootNode = new Node(0)

    def rec(actualMarble: Int, node: Node, acc: List[(Int, Int)]): Map[Int, Long] ={
      if(actualMarble > marbles) {
        acc.groupBy(_._1).mapValues(_.map(_._2.toLong).sum)
      } else if(actualMarble % 23 == 0) {
        val newPos = node.navigateLeft(7)
        val additionalPoint = newPos.value
        val newNewPos = newPos.deleteThisNode()
        val newPoints = (actualMarble % players) -> (actualMarble + additionalPoint) :: acc
        rec(actualMarble + 1, newNewPos, newPoints)
      } else {
        val newPos = node.navigateRight(1).addNewNodeAfterThis(actualMarble)
        rec(actualMarble + 1, newPos, acc)
      }
    }
    rec(1, rootNode, List.empty).maxBy(_._2)._2
  }

  "marble stuff" should {
    "addMarble correct" in {
      val n = new Node(0)
      val n2 = n.addNewNodeAfterThis(1)
      n.toList shouldBe List(0, 1)
      n2.value shouldBe 1
      val n3 = n2.navigateRight(1)
      val n4 = n3.addNewNodeAfterThis(2)
      n.toList shouldBe List(0, 2, 1)
    }
    "removeMarble correct" in {
      val n = new Node(0)
      val n2 = n.addNewNodeAfterThis(1)
      val n3 = n2.addNewNodeAfterThis(2)
      val n4 = n3.deleteThisNode()
      n.toList shouldBe List(0,1)
      n4 shouldBe n
    }
  }

  "d9s1" should {

    "solve examples" in {
      List(
        (9, 25, 32),
        (10, 1618, 8317),
        (13, 7999, 146373),
        (17, 1104, 2764),
        (21, 6111, 54718),
        (30, 5807, 37305)
      ).foreach { case (players, marbles, expected) =>
        d9s1(players, marbles) shouldBe expected
      }
    }

    "solve" in {
      println(d9s1(466 , 71436 ))
    }

  }

  "d9s2 solve" in {
    //slow as hell
    println(d9s1(466 , 71436 * 100 ))
  }

}

class Day9List extends WordSpecLike with Matchers {

  def addMarble(marbles: List[Int], currentIdx: Int, toAdd: Int, step: Int): (List[Int], Int) = {
    val (front,  back, idx) = splitMarbleList(marbles, currentIdx, step)
    (front ++ List(toAdd) ++ back, idx)
  }

  private def splitMarbleList(marbles: List[Int], currentIdx: Int, step: Int) = {
    val insertPos = currentIdx + step
    val insertIdx = if (insertPos > marbles.size) insertPos - marbles.size
    else if (insertPos < 0) insertPos + marbles.size
    else insertPos
    val (front, back) = marbles.splitAt(insertIdx)
    (front, back, insertIdx)
  }

  def removeMarble(marbles: List[Int], currentIdx: Int, step: Int): (Int, List[Int], Int) = {
    val (front,  back, idx) = splitMarbleList(marbles, currentIdx, step)
    (back.head, front ++ back.tail, idx)
  }

  def playersCycle(players: Int): Stream[Int] = {
    lazy val playersC: Stream[Int] = (1 to players).toStream #::: playersC
    playersC
  }

  def d9s1(players: Int, marbles: Int) = {
    val marblesWithPlayers = (0 to marbles).zip(playersCycle(players)).toList

    @tailrec
    def rec(reminder: List[(Int, Int)], marbles: List[Int], pos: Int, acc: List[(Int, Int)]): Map[Int, Int] = {
      reminder match {
        case Nil => acc.groupBy(_._1).mapValues(_.map(_._2).sum)
        case (m, p) :: t if (m % 23) == 0 =>
          val (additionalPoint, newList, newPos) = removeMarble(marbles,pos, -7)
          val newPoints = p -> (m + additionalPoint) :: acc
          rec(t, newList, newPos, newPoints)
        case (m, p) :: t =>
          val (newList, newPos) = addMarble(marbles,pos, m,2)
          rec(t, newList, newPos, acc)
      }
    }

    rec(marblesWithPlayers.tail, List(0), 0, List.empty).maxBy(_._2)._2
  }


  "marble stuff" should {
    "addMarble correct" in {
      addMarble(List(0), 0, 1, 2) shouldBe (List(0, 1), 1)
      addMarble(List(0, 1), 1, 2, 2) shouldBe (List(0, 2,  1), 1)
      addMarble(List(0, 8, 4), 1, 2, 2) shouldBe (List(0, 8,  4, 2), 3)
    }
    "removeMarble correct" in {
      removeMarble(List(0,1,2), 0, -1) shouldBe (2, List(0, 1), 2)
      removeMarble(List(0,1,2), 2, -1) shouldBe (1, List(0, 2), 1)
    }
  }

  "d9s1" should {

    "solve examples" in {
      List((10, 1618, 8317),
        (13, 7999, 146373),
        (17, 1104, 2764),
        (21, 6111, 54718),
        (30, 5807, 37305)
      ).foreach { case (players, marbles, expected) =>
        d9s1(players, marbles) shouldBe expected
      }
    }

    "solve" in {
      println(d9s1(466 , 71436 ))
    }

  }

  "d9s2 solve" in {
    //slow as hell
    println(d9s1(466 , 71436 * 100 ))
  }


}

class Day9ListBuffer extends WordSpecLike with Matchers {
  def addMarble(marbles: ListBuffer[Int], currentIdx: Int, toAdd: Int, step: Int): (ListBuffer[Int], Int) = {
    val idx = splitMarbleList(marbles, currentIdx, step)
    marbles.insert(idx, toAdd)
    (marbles, idx)
  }

  private def splitMarbleList(marbles: Seq[Int], currentIdx: Int, step: Int) = {
    val insertPos = currentIdx + step
    val insertIdx = if (insertPos > marbles.size) insertPos - marbles.size
    else if (insertPos < 0) insertPos + marbles.size
    else insertPos
    insertIdx
  }

  def removeMarble(marbles: ListBuffer[Int], currentIdx: Int, step: Int): (Int, ListBuffer[Int], Int) = {
    val idx = splitMarbleList(marbles, currentIdx, step)
    val removed = marbles(idx)
    marbles.remove(idx)
    (removed, marbles, idx)
  }

  def playersCycle(players: Int): Stream[Int] = {
    lazy val playersC: Stream[Int] = (1 to players).toStream #::: playersC
    playersC
  }

  def d9s1(players: Int, marblesMax: Int) = {
    val s = System.currentTimeMillis()
    val onePercent10 = marblesMax/1000

    @tailrec
    def rec(actualMarble: Int, marbles: ListBuffer[Int], pos: Int, acc: List[(Int, Int)]): Map[Int, Int] = {
      if(actualMarble % onePercent10 == 0) println(s"${actualMarble / onePercent10 /10}% processed, time: ${PuzzleUtil.elapsedFrom(s)}")
      if(actualMarble > marblesMax) {
        acc.groupBy(_._1).mapValues(_.map(_._2).sum)
      } else if(actualMarble % 23 == 0) {
        val (additionalPoint, newList, newPos) = removeMarble(marbles, pos, -7)
        val newPoints = (actualMarble % players) -> (actualMarble + additionalPoint) :: acc
        rec(actualMarble + 1, newList, newPos, newPoints)
      } else {
        val (newList, newPos) = addMarble(marbles, pos, actualMarble, 2)
        rec(actualMarble + 1, newList, newPos, acc)
      }
    }

    rec(1, ListBuffer(0), 0, List.empty).maxBy(_._2)._2
  }


  "marble stuff" should {
    "addMarble correct" in {
      addMarble(ListBuffer(0), 0, 1, 2) shouldBe (ListBuffer(0, 1), 1)
      addMarble(ListBuffer(0, 1), 1, 2, 2) shouldBe (ListBuffer(0, 2,  1), 1)
      addMarble(ListBuffer(0, 8, 4), 1, 2, 2) shouldBe (ListBuffer(0, 8,  4, 2), 3)
    }
    "removeMarble correct" in {
      removeMarble(ListBuffer(0,1,2), 0, -1) shouldBe (2, ListBuffer(0, 1), 2)
      removeMarble(ListBuffer(0,1,2), 2, -1) shouldBe (1, ListBuffer(0, 2), 1)
    }
  }

  "d9s1" should {

    "solve examples" in {
      List((10, 1618, 8317),
        (13, 7999, 146373),
        (17, 1104, 2764),
        (21, 6111, 54718),
        (30, 5807, 37305)
      ).foreach { case (players, marbles, expected) =>
        d9s1(players, marbles) shouldBe expected
      }
    }

    "solve" in {
      println(d9s1(466 , 71436 ))
    }

  }

  "d9s2 solve" in {
    //slow as hell
    println(d9s1(466 , 71436 * 100 ))
  }
}
