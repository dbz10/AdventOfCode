package com.adventofcode
import scala.io.Source
import com.adventofcode.intCode.intCodeComputer._
import com.adventofcode.intCode.utils.replace

object Day2 {

  val inputPath = "inputs/day02_input.txt"

  val input = Source.fromFile(inputPath).getLines().toList.head.split(",").toList.map(_.toInt)

  val outputPartOne = processProgramCode(input)

  println(f"Part One: ${outputPartOne.head}")

  val resultSetPartTwo: Seq[Int] = for {
    noun <- 0 to 99
    verb <- 0 to 99
    if (processProgramCode(
      replace(
        replace(input, 1, noun), 2, verb
      )
    ).head == 19690720)


  } yield 100*noun + verb

  println(f"Part Two: ${resultSetPartTwo}")




}
