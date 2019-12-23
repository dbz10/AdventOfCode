package com.adventofcode.intCode
import com.adventofcode.intCode.utils._
import com.adventofcode.intCode.instruction._
import com.adventofcode.intCode.mode._


object intCodeComputer {

  def processProgramCode(inputList: List[Int]): List[Int] = {
    def go(currentHead: Int, z: List[Int] => List[Int], l: List[Int]): List[Int] => List[Int] = {
      val curOperation = instruction(l(currentHead))
      if ( curOperation == STOP) z
      else {
        val curInstructionLength = numInstructions(curOperation)
        val mP = microProgram(l.slice(currentHead, currentHead+curInstructionLength))
        val codeAction = processMicroprogram(mP)

        go(currentHead + curInstructionLength,
          (lv: List[Int]) => codeAction(z(lv)),
          l)
      }
    }

    go(0, identity[List[Int]], inputList)(inputList)
  }


}
