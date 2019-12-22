package com.adventofcode.intCode

object instruction extends Enumeration {
  type instruction = Value
  val ADD = Value(1, "ADD")
  val MULTIPLY = Value(2, "MULTIPLY")
  val STOP = Value(99, "STOP")
}


