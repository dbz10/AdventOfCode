package com.adventofcode.intCode
import org.scalatest.FunSuite

import com.adventofcode.intCode.instruction._
import com.adventofcode.intCode.mode._
import com.adventofcode.intCode.microProgram._
import com.adventofcode.intCode.utils._

class intCodeTests extends FunSuite{

  test("read an opcode") {

    assert(
      OpCodeSpecification(1) === new OpCodeSpecification(ADD, POSITION, POSITION, POSITION)
    )

    assert(
      OpCodeSpecification(2) === new OpCodeSpecification(MULTIPLY, POSITION, POSITION, POSITION)
    )

    assert(
      OpCodeSpecification(1001) === new OpCodeSpecification(ADD, POSITION, IMMEDIATE, POSITION)
    )

    assert(
      OpCodeSpecification(1002) === new OpCodeSpecification(MULTIPLY, POSITION, IMMEDIATE, POSITION)
    )

    assert(
      OpCodeSpecification(1102) === new OpCodeSpecification(MULTIPLY, IMMEDIATE, IMMEDIATE, POSITION)
    )
  }

  test("read a microprogram") {
    assert(
      microProgram(List(1002,4,3,4))  === new microProgram(
        MULTIPLY, POSITION, 4, IMMEDIATE, 3, POSITION, 4)
    )

    assert(
      microProgram(List(1102,6,2,1))  === new microProgram(
        MULTIPLY, IMMEDIATE, 6, IMMEDIATE, 2, POSITION, 1)
    )

    assert(
      microProgram(List(1101,4,3,4))  === new microProgram(
        ADD, IMMEDIATE, 4, IMMEDIATE, 3, POSITION, 4)
    )

    assert(
      microProgram(List(1099,4,7,4))  === new microProgram(
        STOP, POSITION, 4, IMMEDIATE, 7, POSITION, 4)
    )
  }

  test("generate an add update") {
    val mA = microProgram(List(1001,4,3,4))
    val updateA = addUpdate(mA)
    val testListA = List(1001,4,3,4,33)
    assert (
      updateA(testListA) === List(1001, 4, 3, 4, 36)
    )

    val mB = microProgram(List(1,4,3,4))
    val updateB = addUpdate(mB)
    val testListB = List(1,4,3,4,33)
    assert (
      updateB(testListB) === List(1, 4, 3, 4, 37)
    )

    val mC = microProgram(List(1101,4,3,4))
    val updateC = addUpdate(mC)
    val testListC = List(1,4,3,4,33)
    assert (
      updateC(testListC) === List(1, 4, 3, 4, 7)
    )

  }

  test("generate a multiply update") {
    val mA = microProgram(List(1002,4,3,4))
    val updateA = multiplyUpdate(mA)
    val testListA = List(1002,4,3,4,33)
    assert (
      updateA(testListA) === List(1002, 4, 3, 4, 99)
    )

    val mB = microProgram(List(2,4,3,4))
    val updateB = multiplyUpdate(mB)
    val testListB = List(2,4,3,4,33)
    assert (
      updateB(testListB) === List(2, 4, 3, 4, 132)
    )

    val mC = microProgram(List(1102,4,3,4))
    val updateC = multiplyUpdate(mC)
    val testListC = List(2,4,3,4,33)
    assert (
      updateC(testListC) === List(2, 4, 3, 4, 12)
    )

  }
}
