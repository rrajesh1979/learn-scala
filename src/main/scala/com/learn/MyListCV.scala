package com.learn

object MyListCV extends App {
  val intList: MyList[Int] = new GenericList(1, new GenericList(2, EmptyList))
  println("intList :: " + intList)

  val stringList: MyList[String] = new GenericList[String]("Hello", new GenericList[String]("Scala", EmptyList))
  println("stringList :: " + stringList)

  println(stringList.add(10))
}

abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyList[B]
  def printElements: String
  override def toString: String = "[" + printElements + "]"
}

object EmptyList extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: MyList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def add[B >: Nothing](element: B): MyList[B] = new GenericList(element, EmptyList)

  override def printElements: String = ""
}

class GenericList[+A](h: A, t: MyList[A]) extends MyList[A] {
  override def head: A = h

  override def tail: MyList[A] = t

  override def isEmpty: Boolean = false

  override def add[B >: A](element: B): MyList[B] = new GenericList(element, this)

  override def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements
}