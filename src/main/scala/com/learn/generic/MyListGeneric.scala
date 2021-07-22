package com.learn.generic

abstract class MyListGeneric[+A] {
  def head: A
  def tail: MyListGeneric[A]
  def isEmpty: Boolean
  def add[B >: A](elem: B): MyListGeneric[B]
  def printElements: String
  override def toString: String = "[" + printElements + "]"
}

object EmptyListGeneric extends MyListGeneric[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyListGeneric[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](elem: B): MyListGeneric[B] = new MyListGenericImpl(elem, EmptyListGeneric)
  override def printElements: String = ""
}

class MyListGenericImpl[+A](h: A, t: MyListGeneric[A]) extends MyListGeneric[A] {
  override def head: A = h
  override def tail: MyListGeneric[A] = t
  override def isEmpty: Boolean = false
  override def add[B >: A](elem: B): MyListGeneric[B] = new MyListGenericImpl(elem, this)
  override def printElements: String =
    if (t.isEmpty) h.toString
    else h + ", " + t.printElements
}

object MyListTest extends App {
  val emptyListInt: MyListGeneric[Int] = EmptyListGeneric
  val emptyListString: MyListGeneric[String] = EmptyListGeneric

  val listInt1: MyListGeneric[Int] = new MyListGenericImpl[Int](10, EmptyListGeneric)
  val listInt2: MyListGeneric[Int] = new MyListGenericImpl[Int](10, new MyListGenericImpl[Int](20, EmptyListGeneric))
  val listInt3: MyListGeneric[Int] = new MyListGenericImpl[Int](10, new MyListGenericImpl[Int](20, new MyListGenericImpl[Int](30, EmptyListGeneric)))
  val listInt4 = listInt3.add(40)

  println(listInt1)
  println(listInt2)
  println(listInt3)
  println(listInt4)

}
