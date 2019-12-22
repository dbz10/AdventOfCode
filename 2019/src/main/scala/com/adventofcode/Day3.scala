package com.adventofcode
import scala.io.Source


object Day3 {
  def parseInstruction(word: String): (String, Int) = (word.head.toString, word.tail.toInt)

  def updateVector(currentVector: Vector[(Int,Int)], word: String): Vector[(Int,Int)] = {
    val currentTail: (Int, Int) = currentVector.last
    parseInstruction(word) match {
      case ("R", value) =>
        val extra = for (i <- 1 to value) yield (currentTail._1 + i, currentTail._2)
        currentVector ++ extra

      case ("U", value) =>
        val extra = for (i <- 1 to value) yield (currentTail._1, currentTail._2 + i)
        currentVector ++ extra

      case ("L", value) =>
        val extra = for (i <- 1 to value) yield (currentTail._1 - i, currentTail._2)
        currentVector ++ extra

      case ("D", value) =>
        val extra = for (i <- 1 to value) yield (currentTail._1 , currentTail._2 - i)
        currentVector ++ extra

    }
  }

  val inputPath = "inputs/day03_input.txt"
  val v0 = Vector((0,0))

  val input = for(line <- Source.fromFile(inputPath).getLines())
    yield line.split(",").toList.map(x => x.toString)

  val output = for (line <- input.toList) yield line.foldLeft(v0)(updateVector)
  val jointSet: List[(Int,Int)] = (output(0).tail intersect output(1).tail).toList

  // Part 1
  println(f"Minimum manhattan distance to intersection: ${jointSet.map( x => math.abs(x._1) + math.abs(x._2)).min}")

  // Part 2
  val measuredLines = output.map(x => x.zipWithIndex)

  val stepsToReachPoint = jointSet.map( intersectionPoint => {
    val minStepToReachPoint = measuredLines.map(
      line => line find(_._1 == intersectionPoint) map (_._2) getOrElse(0)
    )

    minStepToReachPoint.sum
  })

  println(f"Minimum steps to reach a line intersection point: ${stepsToReachPoint.filter(_ != 0).min}")

}
