package util

import scala.io.Source

object PuzzleUtil {

  def readInput(year: Int, day: Int):List[String] = {
    val dir = s"y2k${year - 2000}"
    val file = s"d$day.txt"
    Source.fromFile(s"inputs/$dir/$file").getLines.toList
  }

  def measureCodeExTime[B](block: => B): B = {
    val s = System.currentTimeMillis()
    val ret = block
    val elapsed = System.currentTimeMillis() - s
    println(s"$elapsed ms")
    ret
  }

  def msToString(ms: Long) = {
    import java.text.SimpleDateFormat
    import java.util.Date
    new SimpleDateFormat("HH:mm:ss:SSS").format(new Date(ms))
  }

  def elapsedFrom(from: Long) = {
    val elapsed = System.currentTimeMillis() - from
    msToString(elapsed)
  }
}
