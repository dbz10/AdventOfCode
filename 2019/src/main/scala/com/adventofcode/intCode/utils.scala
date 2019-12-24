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

  def processMicroprogram(m: microProgram, cs: state): state = (m.opCode, m) match {
    case (ADD, codeParams) => state(
      addUpdate(codeParams)(cs.currentCode),
      cs.currentPosition + numInstructions(ADD),
      cs.inputs
    )

    case (MULTIPLY, codeParams) => state(
      multiplyUpdate(codeParams)(cs.currentCode),
      cs.currentPosition + numInstructions(MULTIPLY),
      cs.inputs
    )

    case (STOP, _) => state(
      cs.currentCode,
      cs.currentPosition,
      cs.inputs
    )

    case (INPUT, codeParams) => state(
      inputUpdate(codeParams, cs.inputs.head)(cs.currentCode),
      cs.currentPosition + numInstructions(INPUT),
      cs.inputs.tail
    )

    case (OUTPUT, codeParams) => state(
      outputUpdate(codeParams)(cs.currentCode),
      cs.currentPosition + numInstructions(OUTPUT),
      cs.inputs
    )

    case (JumpIfTrue, codeParams) => state(
      cs.currentCode,
      if (interpreted(codeParams,0)(cs.currentCode) != 0) interpreted(codeParams,1)(cs.currentCode) else cs.currentPosition + numInstructions(JumpIfTrue),
      cs.inputs
    )

    case (JumpIfFalse, codeParams) => state(
      cs.currentCode,
      if (interpreted(codeParams,0)(cs.currentCode) == 0) interpreted(codeParams,1)(cs.currentCode) else cs.currentPosition + numInstructions(JumpIfFalse),
      cs.inputs
    )

    case (LESSTHAN, codeParams) => state(
      lessThanUpdate(codeParams)(cs.currentCode),
      cs.currentPosition + numInstructions(LESSTHAN),
      cs.inputs
    )

    case (EQUALS, codeParams) => state(
      equalsUpdate(codeParams)(cs.currentCode),
      cs.currentPosition + numInstructions(EQUALS),
      cs.inputs
    )


  }




  def interpreted(program: microProgram, i: Int): List[Int] => Int = {
    // reads the parameter mode for a variable and returns the correct value for value i
    (program.parameters(i), program.values(i)) match {
      case (POSITION, v) => (l: List[Int]) => l(v)
      case (IMMEDIATE, v) => (l: List[Int]) => v
    }
  }

  def mathUpdate(program: microProgram, op: (Int, Int) => Int): CodeAction =  {
    (l: List[Int]) => replace(l, program.values(2), op(
      interpreted(program,0)(l), interpreted(program,1)(l)
    ))
  }

  def addUpdate(program: microProgram): CodeAction = mathUpdate(program, _ + _)
  def multiplyUpdate(program: microProgram): CodeAction = mathUpdate(program, _ * _)
  def inputUpdate(program: microProgram, input: Int): CodeAction = (l: List[Int]) => replace(l, program.values(0), input)
  def outputUpdate(program: microProgram): CodeAction = (l: List[Int]) => {
    println(f"Output: ${l(program.values(0))}")
    l
  }
  def jumpIfTrueUpdate(program: microProgram): CodeAction = identity[List[Int]]
  def jumpIfFalseUpdate(program: microProgram): CodeAction = identity[List[Int]]
  def lessThanUpdate(program: microProgram): CodeAction = {
    (l: List[Int]) => if (interpreted(program,0)(l) < interpreted(program,1)(l)) replace(l, program.values(2), 1)
    else replace(l, program.values(2), 0)
  }
  def equalsUpdate(program: microProgram): CodeAction = {
    (l: List[Int]) => if (interpreted(program,0)(l) == interpreted(program,1)(l)) replace(l, program.values(2), 1)
    else replace(l, program.values(2), 0)
  }




      val numInstructions = Map(
        STOP -> 1,
        ADD -> 4,
        MULTIPLY -> 4,
        INPUT -> 2,
        OUTPUT -> 2,
        JumpIfTrue -> 3,
        JumpIfFalse -> 3,
        LESSTHAN -> 4,
        EQUALS -> 4
      )



}

case class state(currentCode: List[Int], currentPosition: Int, inputs: List[Int])

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

