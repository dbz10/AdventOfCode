package com.adventofcode
import scala.collection.immutable.HashSet
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

  val inputPath = "/Users/dben-zion/projects/AdventOfCode/2019/inputs/day03_input.txt"
  val v0 = Vector((0,0))


  val input = for(line <- Source.fromFile(inputPath).getLines())
    yield line.split(",").toList.map(x => x.toString)

//  val input = List(
//    List("R8","U5","L5","D3"),
//    List("U7","R6","D4","L4")
//  )

  val output = for (line <- input.toList) yield line.foldLeft(v0)(updateVector).tail.toSet
  val jointSet: List[(Int,Int)] = (output(0) intersect output(1)).toList

  println(jointSet.map( x => math.abs(x._1) + math.abs(x._2)).min)


}
