package com.adventofcode
import scala.io.Source

object Day4 {
  val inputPath = "inputs/day04_input.txt"
  val input: String = Source.fromFile(inputPath).mkString
  val Array(lo, hi) = input.split("-").map(_.toInt)

  def findValidCodes(lo: Int, hi: Int, isValid: String => Boolean): Seq[Int] = for (code <- lo to hi if isValid(code.toString)) yield code

  def isValidPartOne(code: String): Boolean = {
    if (code.toString.length != 6) false
    else if (!isSorted(code)) false
    else if (!hasMatchingAdjacentCharacters(code)) false
    else true
  }

  def isSorted(code: String): Boolean = code == code.split("").sorted.mkString

  def hasMatchingAdjacentCharacters(code: String): Boolean = {
    (for (idx <- 0 until code.length - 1) yield code(idx) == code(idx+1))
        .foldLeft(false)(_ || _)
  }

  println(f"Number of part 1 valid codes between $lo and $hi: ${findValidCodes(lo,hi, isValidPartOne).length}")

  // Part 2

  def isValidPartTwo(code: String): Boolean = {
    if (code.toString.length != 6) false
    else if (!isSorted(code)) false
    else if (!hasIsolatedPair(code)) false
    else true
  }

  def hasIsolatedPair(code: String): Boolean = {
    (for (idx <- 0 until code.length - 1) yield {
      if (idx < code.length - 2) {

        if (idx == 0) {
          code(idx) == code(idx+1) &&
          code(idx+1) != code(idx+2)
        }

        else {
          code(idx) == code(idx+1) &&
          code(idx+1) != code(idx+2) &&
          code(idx-1) != code(idx)
        }

      }
      else code(idx) == code(idx+1) && code(idx-1) != code(idx)
    }
      )
      .foldLeft(false)(_ || _)
  }

  println(f"Number of valid part 2 codes between $lo and $hi: ${findValidCodes(lo,hi, isValidPartTwo).length}")

}
