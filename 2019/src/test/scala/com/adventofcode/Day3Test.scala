package com.adventofcode
import org.scalatest.FunSuite
import com.adventofcode.Day3._

class Day3Test extends FunSuite {

 test("VectorRight") {
   val v0 = Vector((0,0))

   assert(
     updateVector(v0, "R2") === Vector((0,0),(1,0),(2,0))
   )

   assert(
     updateVector(v0, "L2") === Vector((0,0),(-1,0),(-2,0))
   )

   assert(
     updateVector(v0, "U2") === Vector((0,0),(0,1),(0,2))
   )

   assert(
     updateVector(v0, "D2") === Vector((0,0),(0,-1),(0,-2))
   )
 }

  test("VectorCompose") {
    val v0 = Vector((0,0))
    val words = List("R2", "U3")

    assert(
      words.foldLeft(v0)(updateVector) === Vector((0,0),(1,0),(2,0),(2,1),
        (2,2),(2,3))
    )
  }

  test ("findJoints") {
    val v0 = Vector((0,0))
    val input = List(
      List("R8","U5","L5","D3"),
      List("U7","R6","D4","L4")
    )
    val output = for (line <- input) yield line.foldLeft(v0)(updateVector)

    val jointSet = output(0).filter(output(1).contains(_))


  }

}
