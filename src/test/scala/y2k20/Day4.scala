package y2k20

import org.scalatest.{Matchers, WordSpecLike}
import util.PuzzleUtil

import scala.util.Try

class Day4 extends WordSpecLike with Matchers {

  /*
    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID)
   */

  def validatePassport(in: String): Boolean = {
    validateRequiredFields(getPassportFields(in))
  }

  def getPassportFields(in: String): Map[String, String] = {
    val fields = in.split("\n").flatMap(_.split(" ")).filter(_.nonEmpty).toSeq
    fields.map(_.split(":")).map(r => r(0) -> r(1)).toMap
  }

  def validateRequiredFields(in: Map[String, String]): Boolean = {
    val requeired = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    (requeired -- in.keys).isEmpty
  }

  def validate_byr(in: Map[String, String]): Boolean = {
    Try(in("byr").toInt).map(b => 1920 <= b && b <= 2002).getOrElse(false)
  }

  def validate_iyr(in: Map[String, String]): Boolean = {
    Try(in("iyr").toInt).map(b => 2010 <= b && b <= 2020).getOrElse(false)
  }

  def validate_eyr(in: Map[String, String]): Boolean = {
    Try(in("eyr").toInt).map(b => 2020 <= b && b <= 2030).getOrElse(false)
  }

  def validate_hgt(in: Map[String, String]): Boolean = {
    val hgt = in("hgt")
    if(hgt.endsWith("in")) {
      Try(hgt.dropRight(2).toInt).map(b => 59 <= b && b <= 76).getOrElse(false)
    } else if(hgt.endsWith("cm")) {
      Try(hgt.dropRight(2).toInt).map(b => 150 <= b && b <= 193).getOrElse(false)
    } else false
  }
  def validate_hcl(in: Map[String, String]): Boolean = {
    val hcl = in("hcl")
    val valid = ('0' to '9') ++ ('a' to 'f')
    if(hcl.startsWith("#")) {
      hcl.drop(1).forall(valid.contains)
    } else false
  }
  def validate_ecl(in: Map[String, String]): Boolean = {
    val ecl = in("ecl")
    val valid = Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    valid.contains(ecl)
  }
  def validate_pid(in: Map[String, String]): Boolean = {
    val pid = in("pid")
    (pid.size == 9) && Try(pid.toInt).isSuccess
  }

  def validatePassportV2(in: String): Boolean = {
    val f = getPassportFields(in)
    validateRequiredFields(f) &&
      validate_byr(f) &&
      validate_iyr(f) &&
      validate_eyr(f) &&
      validate_hgt(f) &&
      validate_hcl(f) &&
      validate_ecl(f) &&
      validate_pid(f)
  }

  "passport validation is ok" in {
    val passports =
      """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
        |byr:1937 iyr:2017 cid:147 hgt:183cm
        |
        |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
        |hcl:#cfa07d byr:1929
        |
        |hcl:#ae17e1 iyr:2013
        |eyr:2024
        |ecl:brn pid:760753108 byr:1931
        |hgt:179cm
        |
        |hcl:#cfa07d eyr:2025 pid:166559648
        |iyr:2011 ecl:brn hgt:59in
        |""".stripMargin

    val parsedPassports = passports.lines.toSeq.foldLeft(List("")){case (acc, newL) => if(newL.trim.isEmpty) "" :: acc else (acc.head + "\n" + newL) :: acc.tail }.reverse

    //println(parsedPassports)
    validatePassport(parsedPassports(0)) shouldBe true
    validatePassport(parsedPassports(1)) shouldBe false
    validatePassport(parsedPassports(2)) shouldBe true
    validatePassport(parsedPassports(3)) shouldBe false
  }

  "d4s1" in {
    val passports = PuzzleUtil.readInput(2020, 4)

    val parsedPassports = passports.foldLeft(List("")){case (acc, newL) => if(newL.trim.isEmpty) "" :: acc else (acc.head + "\n" + newL) :: acc.tail }.reverse

    println(parsedPassports.count(validatePassport))
  }

  "validate_hgt" in {
      validate_hgt(Map("hgt" -> "60in")) shouldBe true
      validate_hgt(Map("hgt" -> "190cm")) shouldBe true
      validate_hgt(Map("hgt" -> "190in")) shouldBe false
      validate_hgt(Map("hgt" -> "190")) shouldBe false
  }

  "d4s2" in {
    val passports = PuzzleUtil.readInput(2020, 4)

    val parsedPassports = passports.foldLeft(List("")){case (acc, newL) => if(newL.trim.isEmpty) "" :: acc else (acc.head + "\n" + newL) :: acc.tail }.reverse

    println(parsedPassports.count(validatePassportV2))
  }
}
