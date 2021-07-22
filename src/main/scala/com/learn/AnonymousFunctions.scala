package com.learn

object AnonymousFunctions extends App {

  val doublerA = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 * 2
  }
  println(doublerA(2))

  val doublerB: (Int => Int) = (x: Int) => x * 2
  println(doublerB(3))

  val doublerC = (x: Int) => x * 2
  println(doublerC(4))

  val doublerD: (Int => Int) = x => x * 2
  println(doublerC(5))

  val doublerE: Int => Int = _ * 2
  println(doublerE(5))

  val adderA: (Int, Int) => Int = (x, y) => x + y
  println(adderA(5, 6))

  val adderB: (Int, Int) => Int = _ + _
  println(adderB(5, 6))

  val noParamA: () => Int = () => 3
  val noParamB = () => 3

  println(noParamA())
  println(noParamB())

}
