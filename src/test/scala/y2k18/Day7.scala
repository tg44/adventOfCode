package y2k18

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

import scala.annotation.tailrec
import scala.collection.immutable

class Day7 extends WordSpecLike with Matchers {

  def parseStringToDependency(x: String) = {
    //Step C must be finished before step A can begin. => (A -> C)
    x.dropRight(11).takeRight(1)-> x.drop(5).take(1)
  }

  //_1 depends on _2
  def d7s1(deps: List[(String, String)]) = {
    val nodes = deps.flatMap(d => List(d._1, d._2)).toSet
    @tailrec
    def rec(completed: List[String]): List[String] = {
      val filterOutCompleted = deps.filter { case (x, y) => !(completed.contains(x) || completed.contains(y)) }
      val dependingNodes = filterOutCompleted.groupBy(_._1).filter(_._2.nonEmpty).keySet
      ((nodes -- completed) -- dependingNodes).toList match {
        case Nil => completed
        case l => rec(l.min :: completed)
      }
    }
    rec(List.empty).reverse.mkString
  }

  def time(letter: String, additiveTime: Int) = {
    (letter.head - 'A').toInt + 1 + additiveTime
  }

  def d7s2(deps: List[(String, String)], additiveTime: Int, workerNum: Int) = {
    val nodes = deps.flatMap(d => List(d._1, d._2)).toSet

    @tailrec
    def steps(workingOn: List[(String, Int)], leftovers: List[String], i: Int): Int = {
      if(leftovers.isEmpty && workingOn.isEmpty) {
        i
      } else {
        //println(workingOn, i)
        val decWork = workingOn.map{case (id, time) => (id, time-1)}.filter(_._2 != 0)
        val (newWorkingOn, newLeftowers) = taskManager(decWork, leftovers)
        steps(newWorkingOn, newLeftowers, i+1)
      }
    }

    @tailrec
    def taskManager(workingOn: List[(String, Int)], leftovers: List[String]): (List[(String, Int)], List[String]) = {
      if(workingOn.size == workerNum) {
        (workingOn, leftovers)
      } else if(leftovers.isEmpty) {
        (workingOn, leftovers)
      } else {
        val completed = nodes -- leftovers -- workingOn.map(_._1)
        val filterOutCompleted = deps.filter { case (x, y) => !(completed.contains(x) || completed.contains(y)) }
        val filterOutWorkingOn = filterOutCompleted.filter { case (x, y) => !workingOn.map(_._1).contains(x) }
        val dependingNodes = filterOutWorkingOn.groupBy(_._1).filter(_._2.nonEmpty).keySet
        (nodes -- completed -- dependingNodes --  workingOn.map(_._1)).toList match {
          case Nil => (workingOn, leftovers)
          case l => taskManager(l.min -> time(l.min, additiveTime) :: workingOn, leftovers.filter( _ != l.min))
        }
      }
    }

    steps(Nil, nodes.toList, 0) -1 /*initial add*/
  }



  val example = List(
    "Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin.",
  ).map(parseStringToDependency)

  "d7s1" should {
    "solve the example" in {
      d7s1(example) shouldBe "CABDFE"
    }

    "solve" in {
      println(d7s1(PuzzleUtil.readInput(2018, 7).map(parseStringToDependency)))
    }
  }

  "d7s2" should {
    "solve the example" in {
      d7s2(example, 0, 2) shouldBe 15
    }


    "solve" in {
      println(d7s2(PuzzleUtil.readInput(2018, 7).map(parseStringToDependency), 60, 5))
    }
  }

}
