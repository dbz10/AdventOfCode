package com.adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App {

  def fuelSingle(mass: Int): Int = math.floor(mass/3).toInt - 2

  def fuel(mass: Int): Int = {
    @tailrec
    def go(mass: Int, acc: Int): Int = {
      val z = math.floor(mass/3).toInt - 2
      if (z > 0) go(z, acc + z) else acc
    }

    go(mass, 0)
  }

  val inputPath = "/Users/dben-zion/projects/AdventOfCode/2019/inputs/day01_input.txt"

  val input = Source.fromFile(inputPath).getLines.toList


  val resultPartOne = input.map(x => x.toInt).map(fuelSingle).sum

  println(resultPartOne)

  val resultPartTwo = input.map(x => x.toInt).map(fuel).sum

  println(resultPartTwo)

}
