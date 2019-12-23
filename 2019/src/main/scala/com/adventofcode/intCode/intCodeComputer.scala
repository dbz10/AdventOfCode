package com.adventofcode.intCode
import com.adventofcode.intCode.utils._
import com.adventofcode.intCode.instruction._
import com.adventofcode.intCode.microProgram.OpCodeSpecification
import com.adventofcode.intCode.mode._


object intCodeComputer {

  def processProgramCode(code: List[Int], inputs: List[Int] = List(0)): List[Int] = {
    def go(aux: auxiliaries, l: List[Int]): List[Int]  = {
      val curOperation =  OpCodeSpecification(l(aux.currentPosition)).opCode
      if ( curOperation == STOP) l
      else {
        val curInstructionLength = numInstructions(curOperation)
        val mP = microProgram(l.slice(aux.currentPosition, aux.currentPosition+curInstructionLength))
        val (codeAction, newAux) = processMicroprogram(mP, aux)

        go(newAux, codeAction(l))
      }
    }
    val auxZero = auxiliaries(currentPosition = 0, inputs = inputs)
    go(auxZero, code)
  }


}
