package com.learn.hof

abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](elem: B): MyList[B]
  def printElements: String
  override def toString: String = "[" + printElements + "]"

  //anonymous functions
  def map[B](transformer: A => B): MyList[B]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]

  //operators
  def ++[B >: A](list: MyList[B]): MyList[B]

  //HOFs
  def forEach(f: A => Unit): Unit
}

object EmptyList extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing](elem: B): MyList[B] = new MyListImpl(elem, EmptyList)
  override def printElements: String = ""
  override def map[B](transformer: Nothing => B): MyList[B] = EmptyList
  override def flatMap[B](transformer: Nothing => MyList[B]): MyList[B] = EmptyList
  override def filter(predicate: Nothing => Boolean): MyList[Nothing] = EmptyList
  override def ++[B >: Nothing](list: MyList[B]): MyList[B] = list

  override def forEach(f: Nothing => Unit): Unit = ()
}

class MyListImpl[+A](h: A, t: MyList[A]) extends MyList[A] {
  override def head: A = h
  override def tail: MyList[A] = t
  override def isEmpty: Boolean = false

  override def add[B >: A](elem: B): MyList[B] = new MyListImpl(elem, this)

  override def printElements: String =
    if (t.isEmpty) h.toString
    else h + ", " + t.printElements

  override def map[B](transformer: A => B): MyList[B] =
    new MyListImpl(transformer(h), t.map(transformer))

  override def flatMap[B](transformer: A => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)

  override def filter(predicate: A => Boolean): MyList[A] =
    if (predicate(h)) new MyListImpl(h, t.filter(predicate))
    else t.filter(predicate)

  override def ++[B >: A](list: MyList[B]): MyList[B] = new MyListImpl[B](h, t ++ list)

  override def forEach(f: A => Unit): Unit ={
    f(h)
    t.forEach(f)
  }
}

object MyListTest extends App {
  val emptyListInt: MyList[Int] = EmptyList
  val emptyListString: MyList[String] = EmptyList

  val listInt1: MyList[Int] = new MyListImpl[Int](10, EmptyList)
  val listInt2: MyList[Int] = new MyListImpl[Int](10, new MyListImpl[Int](20, EmptyList))
  val listInt3: MyList[Int] = new MyListImpl[Int](10, new MyListImpl[Int](20, new MyListImpl[Int](30, EmptyList)))
  val listInt4 = listInt3.add(40)

  listInt3.forEach(x => println(x))
  listInt4.forEach(print)

}