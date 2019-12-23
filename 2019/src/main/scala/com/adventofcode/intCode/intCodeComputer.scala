package com.adventofcode.intCode
import com.adventofcode.intCode.utils._


object intCodeComputer {

  def processProgramCode(inputList: List[Int]): List[Int] = {
    def go(currentHead: Int, z: List[Int] => List[Int], l: List[Int]): List[Int] => List[Int] = {
      if (l(currentHead) == 99) z
      else {
        val quadruple = l.slice(currentHead, currentHead+4)
        val curInstructionLength = 4
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
