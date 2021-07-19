package com.learn

object MyListCV extends App {
  val intList: MyList[Int] = new GenericList(1, new GenericList(2, new GenericList(3, new GenericList(4, EmptyList))))
  println("intList :: " + intList)

  val anotherIntList: MyList[Int] = new GenericList(100, new GenericList(200, new GenericList(300, new GenericList(400, EmptyList))))
  println("intList :: " + anotherIntList)

  val stringList: MyList[String] = new GenericList[String]("Hello", new GenericList[String]("Scala", EmptyList))
  println("stringList :: " + stringList)

  println(stringList.add(10))

  println(intList ++ anotherIntList)
  println(intList.flatMap(new MyTransformer[Int, MyList[Int]] {
    override def transform(element: Int): MyList[Int] = new GenericList(element, new GenericList(element + 1, EmptyList))
  }))

  //square of number transformer
  print(intList.toString)
  print(" => ")
  println(intList.map(new MyTransformer[Int, Int] {
    override def transform(element: Int): Int = element * element
  }).toString)

  //cube of number transformer
  print(intList.toString)
  print(" => ")
  println(intList.map(new MyTransformer[Int, Int] {
    override def transform(element: Int): Int = element * element * element
  }).toString)

  //filter even numbers
  print(intList.toString)
  print(" => ")
  println(intList.filter(new MyPredicate[Int] {
    override def test(element: Int): Boolean = element % 2 == 0
  }))
}

abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyList[B]
  def printElements: String
  override def toString: String = "[" + printElements + "]"

  def map[B](tansformer: MyTransformer[A,B]): MyList[B]
  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B]
  def filter(predicate: MyPredicate[A]): MyList[A]

  def ++[B >: A](list: MyList[B]): MyList[B]
}

object EmptyList extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: MyList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def add[B >: Nothing](element: B): MyList[B] = new GenericList(element, EmptyList)

  override def printElements: String = ""

  override def map[B](tansformer: MyTransformer[Nothing,B]): MyList[B] = EmptyList
  def flatMap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = EmptyList
  override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = EmptyList

  override def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
}

class GenericList[+A](h: A, t: MyList[A]) extends MyList[A] {
  override def head: A = h

  override def tail: MyList[A] = t

  override def isEmpty: Boolean = false

  override def add[B >: A](element: B): MyList[B] = new GenericList(element, this)

  override def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  override def map[B](transformer: MyTransformer[A,B]): MyList[B] =
    new GenericList(transformer.transform(h), t.map(transformer))

  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] =
    transformer.transform(h) ++ t.flatMap(transformer)

  override def filter(predicate: MyPredicate[A]): MyList[A] =
    if (predicate.test(h)) new GenericList(h, t.filter(predicate))
    else t.filter(predicate)

  override def ++[B >: A](list: MyList[B]): MyList[B] =
    new GenericList(h, t ++ list)

}

trait MyPredicate[-T] {
  def test(element: T): Boolean
}

trait MyTransformer[-A,B] {
  def transform(element: A): B
}