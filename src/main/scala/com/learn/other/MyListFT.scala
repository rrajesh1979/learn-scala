package com.learn.other

object MyListFT extends App {
  val intList: MyListF[Int] = new GenericListF(1, new GenericListF(2, new GenericListF(3, new GenericListF(4, EmptyListF))))
  println("intList :: " + intList)

  val anotherIntList: MyListF[Int] = new GenericListF(100, new GenericListF(200, new GenericListF(300, new GenericListF(400, EmptyListF))))
  println("intList :: " + anotherIntList)

  val stringList: MyListF[String] = new GenericListF[String]("Hello", new GenericListF[String]("Scala", EmptyListF))
  println("stringList :: " + stringList)

  println(stringList.add(10))

  println(intList ++ anotherIntList)
  println(intList.flatMap(elem => new GenericListF(elem, new GenericListF(elem + 1, EmptyListF))))

  //square of number transformer
  print(intList.toString)
  print(" => ")
  println(intList.map(elem => elem * elem).toString)

  //double of number transformer
  print(intList.toString)
  print(" => ")
  println(intList.map(elem => elem * 2).toString)

  //double of number transformer
  print(intList.toString)
  print(" => ")
  println(intList.map(_ * 2).toString)

  //cube of number transformer
  print(intList.toString)
  print(" => ")
  println(intList.map(elem => elem * elem * elem).toString)

  //filter even numbers
  print(intList.toString)
  print(" => ")
  println(intList.filter(elem => elem % 2 == 0))
}

abstract class MyListF[+A] {
  def head: A
  def tail: MyListF[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyListF[B]
  def printElements: String
  override def toString: String = "[" + printElements + "]"

  def map[B](tansformer: A => B): MyListF[B]
  def flatMap[B](transformer: A => MyListF[B]): MyListF[B]
  def filter(predicate: A => Boolean): MyListF[A]

  def ++[B >: A](list: MyListF[B]): MyListF[B]
}

object EmptyListF extends MyListF[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: MyListF[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def add[B >: Nothing](element: B): MyListF[B] = new GenericListF(element, EmptyListF)

  override def printElements: String = ""

  override def map[B](tansformer: Nothing => B): MyListF[B] = EmptyListF
  def flatMap[B](transformer: Nothing => MyListF[B]): MyListF[B] = EmptyListF
  override def filter(predicate: Nothing => Boolean): MyListF[Nothing] = EmptyListF

  override def ++[B >: Nothing](list: MyListF[B]): MyListF[B] = list
}

class GenericListF[+A](h: A, t: MyListF[A]) extends MyListF[A] {
  override def head: A = h

  override def tail: MyListF[A] = t

  override def isEmpty: Boolean = false

  override def add[B >: A](element: B): MyListF[B] = new GenericListF(element, this)

  override def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  override def map[B](transformer: A => B): MyListF[B] =
    new GenericListF(transformer(h), t.map(transformer))

  def flatMap[B](transformer: A => MyListF[B]): MyListF[B] =
    transformer(h) ++ t.flatMap(transformer)

  override def filter(predicate: A => Boolean): MyListF[A] =
    if (predicate(h)) new GenericListF(h, t.filter(predicate))
    else t.filter(predicate)

  override def ++[B >: A](list: MyListF[B]): MyListF[B] =
    new GenericListF(h, t ++ list)

}