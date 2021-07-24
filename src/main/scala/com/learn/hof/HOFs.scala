package com.learn.hof

object HOFs extends App {

  //nTimes(f, n, x) = f(f(...f(x))) = nTimes(f, n-1, f(x))
  //nTimes(f, 3, x) = f(f(f(x)))
  def nTimes(f: Int => Int, n: Int, x: Int): Int =
    if (n <= 0) x
    else nTimes(f, n-1, f(x))

  val plusOne = (x: Int) => x + 1

  println(nTimes(plusOne, 3, 1))

  //ntb(f, x) = x => f(f(...f(x)))
  //increment10 = ntb(plusOne, 10) = plusOne(plusOne(....plusOne(x)))
  //val y = increment10(1)
  def ntb(f: Int => Int, n: Int): (Int => Int) =
    if (n <= 0) f
    else (x: Int) => ntb(f, n-1)(f(x))

  val increment10 = ntb(plusOne, 10)
  println(increment10(10))
}
