package com.adventofcode
import scala.io.Source
import com.adventofcode.intCode.intCodeComputer

object Day5 {
  val inputPath: String = "inputs/day05_input.txt"
  val intCodeProgram = Source.fromFile(inputPath).mkString.split(",").toList.map(_.toInt)

//   Part 1
  intCodeComputer.processProgramCode(intCodeProgram, List(1))

//   Part 2
  intCodeComputer.processProgramCode(intCodeProgram, List(5))
}
