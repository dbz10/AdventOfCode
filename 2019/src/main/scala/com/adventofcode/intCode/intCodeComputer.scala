package com.adventofcode.intCode
import com.adventofcode.intCode.utils._
import com.adventofcode.intCode.instruction._
import com.adventofcode.intCode.microProgram.OpCodeSpecification
import com.adventofcode.intCode.mode._


object intCodeComputer {

  def processProgramCode(code: List[Int], inputValue: Int = 0): List[Int] = {
    def go(currentHead: Int, l: List[Int]): List[Int]  = {
      val curOperation =  OpCodeSpecification(l(currentHead)).opCode
      if ( curOperation == STOP) l
      else {
        val curInstructionLength = numInstructions(curOperation)
        val mP = microProgram(l.slice(currentHead, currentHead+curInstructionLength))
        val codeAction = processMicroprogram(mP, inputValue)

        go(currentHead + curInstructionLength,
          codeAction(l))
      }
    }

    go(0, code)
  }


}
