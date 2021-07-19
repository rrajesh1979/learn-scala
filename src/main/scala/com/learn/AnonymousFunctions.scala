package com.learn

object AnonymousFunctions extends App {

  val doubler = new Function[Int, Int] {
    override def apply(x: Int): Int = x * 2
  }

  val doublerF: (Int => Int) = (x: Int) => x * 2

  val doublerFF: (Int => Int) =
    x => x * 2

  println(doubler(2))

  println(doublerF(5))

  val adder = (a: Int, b: Int) => a + b
  val adderF: (Int, Int) => Int = (a, b) => a + b


}
