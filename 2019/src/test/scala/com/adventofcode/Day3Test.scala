package com.adventofcode
import org.scalatest.FunSuite
import com.adventofcode.Day3.{updateVector}

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

    val jointSet: List[(Int,Int)] = (output(0) intersect output(1)).toList

    assert(jointSet === List((0,0), (6,5), (3,3)))
  }

  test ("countStepsPathOne") {
    val v0 = Vector((0,0))
    val input = List(
      List("R75","D30","R83","U83","L12","D49","R71","U7","L72"),
      List("U62","R66","U55","R34","D71","R55","D58","R83")
    )
    val output = for (line <- input) yield line.foldLeft(v0)(updateVector)
    val jointSet: List[(Int,Int)] = (output(0) intersect output(1)).toList

    val measuredLines = output.map(x => x.zipWithIndex)

    val stepsToReachPoint = jointSet.map( intersectionPoint => {
      val minStepToReachPoint = measuredLines.map(
        line => line find(_._1 == intersectionPoint) map (_._2) getOrElse(0)
      )

      minStepToReachPoint.sum
    })

    assert( stepsToReachPoint.filter(_ != 0).min === 610)

  }

  test ("countStepsPathTwo") {
    val v0 = Vector((0,0))
    val input = List(
      List("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"),
      List("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7")
    )
    val output = for (line <- input) yield line.foldLeft(v0)(updateVector)
    val jointSet: List[(Int,Int)] = (output(0).tail intersect output(1).tail).toList

    println(jointSet)

    val measuredLines = output.map(x => x.zipWithIndex)

    val stepsToReachPoint = jointSet.map( intersectionPoint => {
      val minStepToReachPoint = measuredLines.map(
        line => line find(_._1 == intersectionPoint) map (_._2) getOrElse(0)
      )

      println(minStepToReachPoint)
      minStepToReachPoint.sum
    })

    assert( stepsToReachPoint.filter(_ != 0).min === 410)

  }



}
