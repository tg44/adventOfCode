package util

import scala.io.Source

object PuzzleUtil {

  def readInput(year: Int, day: Int):List[String] = {
    val dir = s"y2k${year - 2000}"
    val file = s"d$day.txt"
    Source.fromFile(s"inputs/$dir/$file").getLines.toList
  }
}
