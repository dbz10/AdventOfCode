package com.adventofcode
import org.scalatest.FunSuite
import com.adventofcode.Day1.{fuel,fuelSingle}


class Day1Test extends FunSuite{
  test("fuelSingle") {
    assert(fuelSingle(100756) === 33583)
    assert(fuelSingle(1969) === 654)
  }

  test("fuel") {
    assert(fuel(100756) === 50346)
    assert(fuel(1969) === 966)
  }
}
