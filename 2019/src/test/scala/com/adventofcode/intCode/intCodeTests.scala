package com.adventofcode.intCode
import org.scalatest.FunSuite

import com.adventofcode.intCode.instruction._
import com.adventofcode.intCode.mode._
import com.adventofcode.intCode.microProgram._
import com.adventofcode.intCode.utils._
import com.adventofcode.intCode.intCodeComputer._

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
        MULTIPLY,
        List(POSITION, IMMEDIATE, POSITION),
        List(4,3,4)
    )
    )

    assert(
      microProgram(List(1102,6,2,1))  === new microProgram(
        MULTIPLY,
        List(IMMEDIATE, IMMEDIATE, POSITION),
        List(6,2,1))
    )

    assert(
      microProgram(List(1101,4,3,4))  === new microProgram(
        ADD,
        List(IMMEDIATE, IMMEDIATE, POSITION),
        List(4,3,4)
      )
    )

    assert(
      microProgram(List(1099,4,7,4))  === new microProgram(
        STOP,
        List(POSITION, IMMEDIATE, POSITION),
        List(4,7,4)
      )
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

  test("interpret a parameter mode") {
    val listy = List(0,1,2,3,4,5)
    val mp = new microProgram(ADD,
      List(POSITION, IMMEDIATE),
      List(4,5)
    )

    assert(
      interpreted(mp, 0)(listy) === 4
    )

    assert(
      interpreted(mp, 1)(listy) === 5
    )

  }

  test("god even knows what is wrong if this test fails"){
    val equalCodeOne = List(3,9,8,9,10,9,4,9,99,-1,8)
    val equalCodeTwo = List(3,3,1108,-1,8,3,4,3,99)
    val lessThanCodeOne = List(3,9,7,9,10,9,4,9,99,-1,8)
    val lessThanCodeTwo = List(3,3,1107,-1,8,3,4,3,99)

    println("The following should output 1 ¯\\_(ツ)_/¯")
    processProgramCode(equalCodeOne, List(8))
    processProgramCode(equalCodeTwo, List(8))
    processProgramCode(lessThanCodeOne, List(5))
    processProgramCode(lessThanCodeTwo, List(4))

    println("The following should output 0 ¯\\_(ツ)_/¯")
    processProgramCode(equalCodeOne, List(5))
    processProgramCode(equalCodeTwo, List(9))
    processProgramCode(lessThanCodeOne, List(10))
    processProgramCode(lessThanCodeTwo, List(11))


  }

}
