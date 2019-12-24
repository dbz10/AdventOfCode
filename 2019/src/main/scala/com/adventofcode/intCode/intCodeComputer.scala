package com.adventofcode.intCode
import com.adventofcode.intCode.utils._
import com.adventofcode.intCode.instruction._
import com.adventofcode.intCode.microProgram.OpCodeSpecification
import com.adventofcode.intCode.mode._


object intCodeComputer {

  def processProgramCode(code: List[Int], inputs: List[Int] = List(0)): List[Int] = {
    def go(cs: state): List[Int]  = {
      val curOperation =  OpCodeSpecification(cs.currentCode(cs.currentPosition)).opCode
      if ( curOperation == STOP) cs.currentCode
      else {
        val curInstructionLength = numInstructions(curOperation)
        val mP = microProgram(cs.currentCode.slice(cs.currentPosition, cs.currentPosition + curInstructionLength))
        val newState = processMicroprogram(mP, cs)

        go(newState)
      }
    }
    val initState = state(currentCode = code, currentPosition = 0, inputs = inputs)
    go(initState)
  }


}
