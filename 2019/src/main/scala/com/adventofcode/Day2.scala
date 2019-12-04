package com.adventofcode

import scala.io.Source

object Day2 {
  def replace[A](l: List[A], idx: Int, vprime: A): List[A] = l.zipWithIndex
    .map( _ match {
      case (v, i) if (i==idx)  => vprime
      case (v, _) => v
    })

    def processQuadruple(quad: List[Int], fullList: List[Int]): List[Int] = quad match {
      case List(1,p1,p2,pf) => replace(fullList, pf, fullList(p1) + fullList(p2))
      case List(2,p1,p2,pf) => replace(fullList, pf, fullList(p1) * fullList(p2))
      case List(99,p1,p2,pf) => fullList
    }

  def processCode(l: List[Int]): List[Int] = {
    def go(position: Int, l: List[Int]): List[Int] = {
      if (l(position) == 99) l else if (position < l.length) go(position+4, processQuadruple(l.slice(position, position+4), l)) else
        processQuadruple(l.slice(position, position+4), l)
    }


    go(0, l)
  }

  val inputPath = "/Users/dben-zion/projects/AdventOfCode/2019/inputs/day02_input.txt"

  val input = Source.fromFile(inputPath).getLines().toList.head.split(",").toList.map(_.toInt)

  val outputPartOne = processCode(input)

  println(outputPartOne.head)

  val resultSetPartTwo = for {
    noun <- 0 to 99
    verb <- 0 to 99
    if (processCode(
      replace(
        replace(input, 1, noun), 2, verb
      )
    ).head == 19690720)

  } yield 100*noun + verb

  println(resultSetPartTwo)




}
