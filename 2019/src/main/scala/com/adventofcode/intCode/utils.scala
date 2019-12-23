package com.adventofcode.intCode

import com.adventofcode.intCode.instruction._
import com.adventofcode.intCode.mode._


object utils {
  type CodeAction = List[Int] => List[Int]

  def replace[A](l: List[A], idx: Int, vprime: A): List[A] = l.zipWithIndex
    .map( _ match {
      case (v, i) if (i==idx)  => vprime
      case (v, _) => v
    })

  def processMicroprogram(m: microProgram): CodeAction = (m.opCode, m) match {
    case (ADD, codeParams) => addUpdate(codeParams)
    case (MULTIPLY, codeParams) => multiplyUpdate(codeParams)
    case (STOP, _) => identity[List[Int]]
  }

  def addUpdate(program: microProgram): CodeAction =  (program.paramOneMode, program.paramTwoMode) match {
    case (POSITION, POSITION) => (l: List[Int]) => replace(l,
      program.paramThreeValue,
      l(program.paramOneValue) + l(program.paramTwoValue))

    case (POSITION, IMMEDIATE) => (l: List[Int]) => replace(l,
      program.paramThreeValue,
      l(program.paramOneValue) + program.paramTwoValue)

    case (IMMEDIATE, POSITION) => (l: List[Int]) => replace(l,
      program.paramThreeValue,
      program.paramOneValue + l(program.paramTwoValue))

    case (IMMEDIATE, IMMEDIATE) => (l: List[Int]) => replace(l,
      program.paramThreeValue,
      program.paramOneValue + program.paramTwoValue)

  }

  def multiplyUpdate(program: microProgram): CodeAction = (program.paramOneMode, program.paramTwoMode) match {
    case (POSITION, POSITION) => (l: List[Int]) => replace(l,
      program.paramThreeValue,
      l(program.paramOneValue) * l(program.paramTwoValue))

    case (POSITION, IMMEDIATE) => (l: List[Int]) => replace(l,
      program.paramThreeValue,
      l(program.paramOneValue) * program.paramTwoValue)

    case (IMMEDIATE, POSITION) => (l: List[Int]) => replace(l,
      program.paramThreeValue,
      program.paramOneValue * l(program.paramTwoValue))

    case (IMMEDIATE, IMMEDIATE) => (l: List[Int]) => replace(l,
      program.paramThreeValue,
      program.paramOneValue * program.paramTwoValue)

  }

  def processProgramCode(inputList: List[Int]): List[Int] = {
    def go(currentHead: Int, z: List[Int] => List[Int], l: List[Int]): List[Int] => List[Int] = {
      if (l(currentHead) == 99) z
      else {
        val quadruple = l.slice(currentHead, currentHead+4)
        val curInstructionLength = quadruple.mkString.length
        val mP = microProgram(l.slice(currentHead, currentHead+4))
        val codeAction = processMicroprogram(mP)

        go(currentHead + curInstructionLength,
          (lv: List[Int]) => codeAction(z(lv)),
          l)
      }
    }

    go(0, identity[List[Int]], inputList)(inputList)
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

object microProgram {
  def apply(l: List[Int]): microProgram = {
   val opCodeSpec = OpCodeSpecification(l(0))

    new microProgram(opCodeSpec.opCode,
      opCodeSpec.modeOne,
      paramOneValue = l(1),
      opCodeSpec.modeTwo,
      paramTwoValue = l(2),
      opCodeSpec.modeThree,
      paramThreeValue = l(3))
  }


  case class OpCodeSpecification(opCode: instruction,
                                 modeOne: mode,
                                 modeTwo: mode,
                                 modeThree: mode)

  object OpCodeSpecification {
    def apply(i: Int): OpCodeSpecification = {
      val filledCode: String = "%05d".format(i)


      val operationPart = filledCode.slice(3,3+2).toInt

      new OpCodeSpecification(
        instruction(operationPart),
        mode(filledCode(2).asDigit),
        mode(filledCode(1).asDigit),
        mode(filledCode(0).asDigit)
      )
    }
  }
}

