package com.learn.cf

object MyCurriedFunction extends App {
  val superAdder: Function1[Int, Function1[Int, Int]] = new Function1[Int, Function1[Int, Int]] {
    override def apply(x: Int): Function1[Int, Int] = new Function1[Int, Int] {
      override def apply(y: Int): Int = x + y
    }
  }

  val adder = superAdder(3)
  println(adder(4))
  println(superAdder(3)(4))

  val superAdderF = (x: Int) => (y: Int) => x + y


  def curriedFormatter(f: String)(x: Double): String = f.format(x)

  val standardFormatter: (Double => String) = curriedFormatter("%4.2f")
  val preciseFormatter: (Double => String) = curriedFormatter("%18.5f")

  println(standardFormatter(Math.PI))
  println(preciseFormatter(Math.PI))
}
