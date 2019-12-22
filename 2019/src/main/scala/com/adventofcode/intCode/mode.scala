package com.adventofcode.intCode

object mode extends Enumeration {
  type mode = Value
  val POSITION = Value(0, "POSITION")
  val IMMEDIATE = Value(1, "IMMEDIATE")
}
