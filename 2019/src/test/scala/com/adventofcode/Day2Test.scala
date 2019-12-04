package com.adventofcode
import org.scalatest.FunSuite
import com.adventofcode.Day2._

class Day2Test extends FunSuite{
  test("processQuadruple") {
    val list1 = List(1,0,1,3)
    val list2 = List(1,1,2,7,10,20,30,40)

    val list3 = List(2,0,1,3)
    val list4 = List(2,1,2,7,10,20,30,40)

    assert(
      processQuadruple(list1.slice(0,4), list1) === List(1,0,1,1)
    )

    assert(
      processQuadruple(list2.slice(0,4), list2) === List(1,1,2,7,10,20,30,3)
    )

    assert(
      processQuadruple(list3.slice(0,4), list3) === List(2,0,1,0)
    )

    assert(
      processQuadruple(list4.slice(0,4), list4) === List(2,1,2,7,10,20,30,2)
    )

  }

  test("processOpCode") {
    assert (
      processCode(List(1,0,0,0,99)) === List(2,0,0,0,99)
    )

    assert (
      processCode(List(2,3,0,3,99)) === List( 2,3,0,6,99)
    )

    assert (
      processCode(List(2,4,4,5,99,0)) === List(2,4,4,5,99,9801)
    )

    assert (
      processCode(List(1,1,1,4,99,5,6,0,99)) === List(30,1,1,4,2,5,6,0,99)
    )
  }
}
