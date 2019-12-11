package y2k19

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

class Day6 extends WordSpecLike with Matchers {

  def parseLine(s: String) = {
    val a = s.split("\\)")
    (a(0), a(1))
  }

  def d6s1(lines: List[String]): Int = {

    val parsed = lines.map(parseLine)

    @scala.annotation.tailrec
    def rec(acc: Map[String, Int], orbits: Seq[(String, String)]): Map[String, Int] = {
      val known = orbits.filter(o => acc.contains(o._1))
      val news = known.map(o => o._2 -> (acc(o._1)+1))
      val rem = orbits.filter(o => !acc.contains(o._1))
      if(rem.isEmpty){
        acc ++ news
      } else {
        rec(acc ++ news, rem)
      }
    }

    val roots = parsed.filter(o => !parsed.exists(_._2 == o._1))
    rec(roots.map(_._1 -> 0).toMap, parsed).values.sum
  }

  "d6s1" should {
    "example" in {
      val input = List(
      "COM)B",
      "B)C",
      "C)D",
      "D)E",
      "E)F",
      "B)G",
      "G)H",
      "D)I",
      "E)J",
      "J)K",
      "K)L",
      )
      d6s1(input) shouldBe 42
    }
    "solve" in {
      println(d6s1(PuzzleUtil.readInput(2019, 6)))
    }

  }

  //this is pretty bad to build with immutability from top to bottom...
  case class Node(name: String, children: Seq[Node])

  def buildTree(orbits: Seq[(String, String)]) = {
    def rec(acc: Seq[Node], orbits: Seq[(String, String)]): Seq[Node] = {
      val endpositions = orbits.filter(o => !orbits.exists(_._1 == o._2 ))
      val remOrbits = orbits.filter(o => orbits.exists(_._1 == o._2 ))
      val accStay = acc.filter(n => !endpositions.exists(_._2 == n.name))
      val newAccMembers = endpositions.map(o => Node(o._1, acc.filter(n => o._2 == n.name)))
      val addedAcc = accStay ++ newAccMembers
      val mergedAcc = addedAcc.map(a => Node(a.name, addedAcc.filter(_.name == a.name).flatMap(_.children))).distinct
      if(remOrbits.isEmpty) {
        mergedAcc
      } else {
        rec(mergedAcc, remOrbits)
      }
    }

    rec(orbits.filter(o => !orbits.exists(_._1 == o._2 )).map(o => Node(o._2, Seq.empty)), orbits)
  }

  def printTree(nodes: Seq[Node], indent: Int = 0): Unit = {
    nodes.foreach{n =>
      println(" "*indent + n.name)
      printTree(n.children, indent + 2)
    }
  }

  "buildTree + printTree" should {
    "work with example" in {
      val input = List(
        "COM)B",
        "B)C",
        "C)D",
        "D)E",
        "E)F",
        "B)G",
        "G)H",
        "D)I",
        "E)J",
        "J)K",
        "K)L",
      )
      printTree(buildTree(input.map(parseLine)))
    }
  }

  def findWithRet[A, B](s: Seq[A])(f: A => B)(p: B => Boolean): Option[(A, B)] = {
    var these = s
    while (!these.isEmpty) {
      val h = f(these.head)
      if (p(h)) return Some(these.head -> h)
      these = these.tail
    }
    None
  }

  def d6s2(lines: List[String]) = {
    //problematic with huge trees
    def find(t: Node, s: String): Boolean = {
      if(t.name == s) {
        true
      } else {
        t.children.exists(find(_, s))
      }
    }

    def findWith2(t: Node, s1: String, s2: String): Node = {
      t.children.find(c => find(c, s1) && find(c, s2)).fold{
        t
      }{ c =>
        findWith2(c, s1, s2)
      }
    }

    def countDistance(t: Node, s: String, acc: Int = 0): Int = {
      if(t.name == s) {
        acc
      } else if(t.children.isEmpty) {
        0
      } else {
        findWithRet(t.children)(countDistance(_, s, acc+1))(_ > 0).map(_._2).getOrElse(0)
      }
    }

    val parsed = lines.map(parseLine)
    val trees = buildTree(parsed)
    trees.find(t => find(t, "YOU") && find(t, "SAN")).map{ tree =>
      val bestSub = findWith2(tree, "YOU", "SAN")
      countDistance(bestSub, "YOU") + countDistance(bestSub, "SAN") -2
    }
  }

  "d6s2" should {
    "work with example" in {
      val input = List(
      "COM)B",
      "B)C",
      "C)D",
      "D)E",
      "E)F",
      "B)G",
      "G)H",
      "D)I",
      "E)J",
      "J)K",
      "K)L",
      "K)YOU",
      "I)SAN",
      )
      d6s2(input) shouldBe Some(4)
    }
    "solve" in {
      println(d6s2(PuzzleUtil.readInput(2019, 6)))
    }
  }

}
