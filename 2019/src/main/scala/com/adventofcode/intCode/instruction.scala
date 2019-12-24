package com.adventofcode.intCode

object instruction extends Enumeration {
  type instruction = Value
  val ADD = Value(1, "ADD")
  val MULTIPLY = Value(2, "MULTIPLY")
  val STOP = Value(99, "STOP")
  val INPUT = Value(3, "INPUT")
  val OUTPUT = Value(4, "OUTPUT")
  val JumpIfTrue = Value(5, "JumpIfTrue")
  val JumpIfFalse = Value(6, "JumpIfFalse")
  val LESSTHAN = Value(7, "LESSTHAN")
  val EQUALS = Value(8, "EQUALS")
}


