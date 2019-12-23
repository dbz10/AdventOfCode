package com.adventofcode
import com.adventofcode.Day2.input
import com.adventofcode.intCode.intCodeComputer.processProgramCode
import com.adventofcode.intCode.utils.replace
import org.scalatest.FunSuite

import scala.io.Source

class Day2Test extends FunSuite {
    // intcode is now separately tested, so here just regressions
  test("Part One") {

    val outputPartOne = processProgramCode(input)

    assert (outputPartOne.head === 3562672)
  }

  test( "Part Two") {
    val resultSetPartTwo: Seq[Int] = for {
      noun <- 0 to 99
      verb <- 0 to 99
      if (processProgramCode(
        replace(
          replace(input, 1, noun), 2, verb
        )
      ).head == 19690720)


    } yield 100*noun + verb

    assert( resultSetPartTwo(0) === 8250)
  }

}
