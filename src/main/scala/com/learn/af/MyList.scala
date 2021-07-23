package com.learn.af

abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](elem: B): MyList[B]
  def printElements: String
  override def toString: String = "[" + printElements + "]"
  def map[B](transformer: A => B): MyList[B]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]
  def ++[B >: A](list: MyList[B]): MyList[B]
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
}

object MyListTest extends App {
  val emptyListInt: MyList[Int] = EmptyList
  val emptyListString: MyList[String] = EmptyList

  val listInt1: MyList[Int] = new MyListImpl[Int](10, EmptyList)
  val listInt2: MyList[Int] = new MyListImpl[Int](10, new MyListImpl[Int](20, EmptyList))
  val listInt3: MyList[Int] = new MyListImpl[Int](10, new MyListImpl[Int](20, new MyListImpl[Int](30, EmptyList)))
  val listInt4 = listInt3.add(40)

  println(listInt1)
  println(listInt2)
  println(listInt3)
  println(listInt4)

  val listInt5 = listInt4.add("50")
  println(listInt5)

  val transformedList3 = listInt3.map(elem => elem * 2)
  println(transformedList3)
  println(listInt3.map(_ * 2))

  val listInt6: MyList[Int] = new MyListImpl[Int](1, new MyListImpl[Int](2, new MyListImpl[Int](3, new MyListImpl[Int](4, new MyListImpl[Int](5, new MyListImpl[Int](6, EmptyList))))))
  println(listInt6)
  val filteredList6 = listInt6.filter(elem => elem % 2 == 0)
  println(filteredList6)
  println(listInt6.filter(_ % 2 == 0))

  val listInt7: MyList[Int] = new MyListImpl[Int](1, new MyListImpl[Int](2, new MyListImpl[Int](3, EmptyList)))
  val listInt8: MyList[Int] = new MyListImpl[Int](4, new MyListImpl[Int](5, EmptyList))

  println(listInt7 ++ listInt8)
  val flatMappedList9 = listInt7.flatMap(elem => (new MyListImpl[Int](elem, new MyListImpl[Int](elem + 2, EmptyList))))

  val flatMappedList10 = listInt7.flatMap(elem => (new MyListImpl[Int](elem, new MyListImpl[Int](elem + 2, EmptyList))))

  println(flatMappedList9)
  val flatMappedList11 = listInt7.flatMap(
    elem => (new MyListImpl[Int](elem, new MyListImpl[Int](elem * 2, EmptyList)))
  )

  println(flatMappedList10)
  println(flatMappedList11)

}