package com.adventofcode.intCode

import com.adventofcode.intCode.instruction._
import com.adventofcode.intCode.mode._

object utils {
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
}

case class microProgram(opCode: instruction,
                        paramOneMode: mode,
                        paramOneValue: Int,
                        paramTwoMode: mode,
                        paramTwoValue: Int,
                        paramThreeMode: mode,
                        paramThreeValue: Int
                       )