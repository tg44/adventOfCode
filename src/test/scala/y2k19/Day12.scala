package y2k19

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

class Day12 extends WordSpecLike with Matchers {

  case class Vec(x: Int, y: Int, z: Int)
  object Vec{
    val empty = Vec(0,0,0)
  }

  def add(v1: Vec, v2: Vec) = Vec(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)

  def energy(v: Vec): Int = Math.abs(v.x) + Math.abs(v.y) + Math.abs(v.z)


  def calculateGravity(v: Vec, other: Vec): Vec = {
    Vec(norm(v.x, other.x), norm(v.y, other.y), norm(v.z, other.z))
  }

  def calculateGravity(v: Vec, others: Seq[Vec]): Vec = {
    others.fold(Vec.empty){case (acc, n) => add(acc, calculateGravity(v, n))}
  }

  def norm(i: Int, j: Int): Int = {
    if(i>j) -1 else if(i<j) 1 else 0
  }

  def calculateGravityFaster(v: Vec, others: List[Vec]): Vec = {
    @scala.annotation.tailrec
    def rec(x: Int, y: Int, z: Int, left: List[Vec]): Vec = {
      left match {
        case Nil => Vec(x,y,z)
        case h :: t =>
          rec(x+norm(v.x, h.x), y+norm(v.y, h.y), z+norm(v.z, h.z), t)
      }
    }
    rec(0,0,0,others)
  }

  "calculateGravity" should {
    "work in simple case" in {
      calculateGravity(Vec(-10,10,5), Vec(10, -10, 5)) shouldBe Vec(1, -1, 0)
    }
    "work in multiple vecs" in {
      calculateGravity(Vec(-10,10,5), Seq(Vec(10, -10, 5), Vec(10, 20, -5))) shouldBe Vec(2, 0, -1)
    }
  }

  def simulate(initial: List[(Vec, Vec)]): List[(Vec, Vec)] = {
    val celestials = initial.map(_._1)
    initial.map{ case (pos, vel) =>
      val g = calculateGravityFaster(pos, celestials)
      val newVel = add(vel, g)
      val newPos = add(pos, newVel)
      (newPos, newVel)
    }
  }

  def simulateN(initial: List[(Vec, Vec)], n: Int): List[(Vec, Vec)] = {
    (1 to n).foldLeft(initial){case (data, _) => simulate(data)}
  }

  "simulate" should {
    "work on example" in {
      val initPos = Seq(
        Vec(-8, -10, 0),
        Vec(5, 5, 10),
        Vec(2, -7, 3),
        Vec(9, -8, -3),
      )
      val init = initPos.map(_ -> Vec.empty).toList
      val out = Seq(
        Vec(  8, -12, -9)->Vec( -7,   3,  0),
        Vec( 13,  16, -3)->Vec(  3, -11, -5),
        Vec(-29, -11, -1)->Vec( -3,   7,  4),
        Vec( 16, -13, 23)->Vec(  7,   1,  1),
      )
      simulateN(init, 100) shouldBe out
    }

    "work on part2 example" in {
      val initPos = Seq(
        Vec(-1, 0, 2),
        Vec(2, -10, -7),
        Vec(4, -8, 8),
        Vec(3, 5, -1),
      )
      val init = initPos.map(_ -> Vec.empty).toList
      val out = simulateN(init, 2772)
      out.map(_._1) shouldBe initPos
    }
  }


  def d12s1(initPositions: Seq[Vec], steps: Int) = {
    val init = initPositions.map(_ -> Vec.empty).toList
    val endState = simulateN(init, steps)
    endState.map{case (pos, vel) => energy(pos) * energy(vel)}.sum
  }

  "d12s1" should {
    "calc ex correctly" in {
      val initPos = Seq(
        Vec(-8, -10, 0),
        Vec(5, 5, 10),
        Vec(2, -7, 3),
        Vec(9, -8, -3),
      )
      d12s1(initPos, 100) shouldBe 1940
    }
    "solve" in {
      val initPos = Seq(
        Vec(-7, -8, 9),
        Vec(-12, -3, -4),
        Vec(6, -17, -9),
        Vec(4, -10, -6),
      )
      println(d12s1(initPos, 1000))
    }
  }

  def d12s2(initPositions: Seq[Vec]): Long = {
    val init = initPositions.map(_ -> Vec.empty).toList
    val from = System.currentTimeMillis()
    @scala.annotation.tailrec
    def rec(initial: List[(Vec, Vec)], n: Long = 0): Long = {
      if(n%(1000000L) == 0) println(s"$n: ${PuzzleUtil.elapsedFrom(from)}")
      val s = simulate(initial)
      if(s == init) {
        n+1
      } else {
        rec(s, n+1)
      }
    }
    rec(init)
  }

  "d12s2" should {
    "calc example1" in {
      val initPos = Seq(
        Vec(-1,  0,  2),
        Vec( 2,-10, -7),
        Vec( 4, -8,  8),
        Vec( 3,  5, -1),
      )
      val out = PuzzleUtil.measureCodeExTime(d12s2(initPos))
      out shouldBe 2772
    }
    "calc example2" in {
      val initPos = Seq(
        Vec(-8, -10, 0),
        Vec(5, 5, 10),
        Vec(2, -7, 3),
        Vec(9, -8, -3),
      )
      //this is ~20 min on my mbp (1135537 ms)
      //the map and the calcGravityFaster are the 2 slowest part which is kinda fine
      //val out = PuzzleUtil.measureCodeExTime(d12s2(initPos))
      //out shouldBe 4686774924L
    }
    "solve" in {
      val initPos = Seq(
        Vec(-7, -8, 9),
        Vec(-12, -3, -4),
        Vec(6, -17, -9),
        Vec(4, -10, -6),
      )
      //nope this will definitely not work 101480000000: 05:28:43:771 :(
      //val out = PuzzleUtil.measureCodeExTime(d12s2(initPos))
      //println(out)
    }
  }


}
