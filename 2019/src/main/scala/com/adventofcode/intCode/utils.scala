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

  def mathUpdate(program: microProgram, op: (Int, Int) => Int): CodeAction =  (program.parameters(0), program.parameters(1)) match {
    case (POSITION, POSITION) => (l: List[Int]) => replace(l,
      program.values(2),
      op(l(program.values(0)), l(program.values(1)))
    )

    case (POSITION, IMMEDIATE) => (l: List[Int]) => replace(l,
      program.values(2),
      op(l(program.values(0)), program.values(1))
    )

    case (IMMEDIATE, POSITION) => (l: List[Int]) => replace(l,
      program.values(2),
      op(program.values(0), l(program.values(1)))
    )

    case (IMMEDIATE, IMMEDIATE) => (l: List[Int]) => replace(l,
      program.values(2),
      op(program.values(0), program.values(1))
    )

  }

  def addUpdate(program: microProgram): CodeAction = mathUpdate(program, _ + _)
  def multiplyUpdate(program: microProgram): CodeAction = mathUpdate(program, _ * _)


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

  val numInstructions = Map(
    STOP -> 1,
    ADD -> 4,
    MULTIPLY -> 4,
    INPUT -> 2,
    OUTPUT -> 2
  )


}

case class microProgram(opCode: instruction,
                        parameters: List[mode],
                        values: List[Int]
                       )

object microProgram {
  def apply(l: List[Int]): microProgram = {
   val opCodeSpec = OpCodeSpecification(l(0))

    new microProgram(opCodeSpec.opCode,
      List(opCodeSpec.modeOne, opCodeSpec.modeTwo, opCodeSpec.modeThree),
      l.tail
    )
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

