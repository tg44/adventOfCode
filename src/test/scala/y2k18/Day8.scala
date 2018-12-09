package y2k18

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

class Day8 extends WordSpecLike with Matchers {

  case class Node(childs: List[Node], meta: List[Int]) {
    lazy val value: Int = {
      if(childs.isEmpty) {
        meta.sum
      } else {
        meta.map(m => childs.lift(m - 1).getOrElse(Node.empty).value).sum
      }
    }
  }

  object Node {
    lazy val empty = Node(List.empty, List.empty)
  }

  def parseInput(numbers: List[Int]) = {
    def getAFullNode(reminder: List[Int]): (Node, List[Int]) = {
      val nodeSize = reminder.head
      val metaSize = reminder.tail.head
      val (nodes, smallerList) = parseChildNodes(nodeSize, reminder.tail.tail)
      val meta = smallerList.take(metaSize)
      (Node(nodes, meta), smallerList.drop(metaSize))
    }

    def parseChildNodes(size: Int, list: List[Int], acc: List[Node] = List.empty): (List[Node], List[Int]) = {
      if(size>0) {
        val (node, reminder) = getAFullNode(list)
        parseChildNodes(size - 1, reminder, node :: acc)
      } else {
        (acc.reverse, list)
      }
    }

    getAFullNode(numbers)._1
  }

  def getMeta(root: Node): List[Int] = {
    root.meta ++ root.childs.flatMap(getMeta)
  }

  def d8s1(numbers: List[Int]) = {
    val root = parseInput(numbers)
    getMeta(root).sum
  }

  def d8s2(numbers: List[Int]) = {
    val root = parseInput(numbers)
    root.value
  }


  "d8s1" should {

    "solve the example" in {
      d8s1("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2".split(" ").map(_.toInt).toList) shouldBe 138
    }

    "solve" in {
      println(d8s1(PuzzleUtil.readInput(2018, 8).head.split(" ").map(_.toInt).toList))
    }

  }

  "d8s2" should {

    "solve the example" in {
      d8s2("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2".split(" ").map(_.toInt).toList) shouldBe 66
    }

    "solve" in {
      println(d8s2(PuzzleUtil.readInput(2018, 8).head.split(" ").map(_.toInt).toList))
    }

  }
}
