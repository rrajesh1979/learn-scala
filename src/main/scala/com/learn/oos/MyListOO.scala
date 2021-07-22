package com.learn.oos

abstract class MyListOO {
  def head: Int
  def tail: MyListOO
  def isEmpty: Boolean
  def add(elem: Int): MyListOO
  def addFirst(elem: Int): MyListOO
  def printElements: String
  override def toString: String = "[" + printElements + "]"
}

object EmptyListOO extends MyListOO {
  override def head: Int = throw new NoSuchElementException
  override def tail: MyListOO = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add(elem: Int): MyListOO = new MyListImplOO(elem, EmptyListOO)
  override def addFirst(elem: Int): MyListOO = new MyListImplOO(elem, EmptyListOO)
  override def printElements: String = ""
}

class MyListImplOO(h: Int, t: MyListOO) extends MyListOO {
  override def head: Int = h
  override def tail: MyListOO = t
  override def isEmpty: Boolean = false
  override def add(elem: Int): MyListOO = new MyListImplOO(elem, this)
  override def addFirst(elem: Int): MyListOO = new MyListImplOO(elem, this)
  override def printElements: String =
    if (t.isEmpty) h.toString
    else h + ", " + t.printElements
}

object MyListTest extends App {
  val listA = new MyListImplOO(10, EmptyListOO)
  println(listA)

  val listB = new MyListImplOO(10, new MyListImplOO(20, new MyListImplOO(30, EmptyListOO)))
  val listC = listB.add(40)
  println(listB)
  println(listC)
}