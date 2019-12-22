package com.adventofcode
import org.scalatest.FunSuite
import com.adventofcode.Day4._

class Day4Test extends FunSuite {

  test("AbleToTraverseCodes") {
    val lo = 100000
    val hi = 200000

    val validCodes = findValidCodes(lo,hi, isValidPartOne)

    assert(validCodes.nonEmpty)
  }
  test("checkSorted") {
    assert(isSorted("123456"))
  }

  test("hasMatchingCharacters") {
    assert(hasMatchingAdjacentCharacters("1223"))
    assert(hasMatchingAdjacentCharacters("123445"))
    assert(hasMatchingAdjacentCharacters("4829938199"))
    assert(!hasMatchingAdjacentCharacters("12345"))
    assert(!hasMatchingAdjacentCharacters("1234530"))
  }

  test("ableToCheckCode") {
    assert(isValidPartOne("5") === false)
    assert(isValidPartOne("10000") === false)
    assert(isValidPartOne("100000") === false)
    assert(isValidPartOne("111111") === true)
    assert(isValidPartOne("211111") === false)
    assert(isValidPartOne("123456") === false)
    assert(isValidPartOne("122345") === true)
    assert(isValidPartOne("123451") === false)
    assert(isValidPartOne("145589") === true)
  }

  test("hasIsolatedPair") {
    assert(hasIsolatedPair(("112345")))
    assert(hasIsolatedPair(("12234")))
    assert(hasIsolatedPair(("1582772399")))
    assert(!hasIsolatedPair(("09823")))
    assert(!hasIsolatedPair(("111234")))
    assert(hasIsolatedPair(("1122234")))
    assert(!hasIsolatedPair(("8238947")))
    assert(!hasIsolatedPair("123444"))
    assert(hasIsolatedPair("111122"))
  }

}
