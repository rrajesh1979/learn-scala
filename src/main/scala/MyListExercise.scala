object MyListExercise extends App {
  println(EmptyList)
  val list = new IntList(5, EmptyList)
  println(list.add(10))
  val newList = new IntList(1, new IntList(2, new IntList(3, EmptyList)))
  val anotherList = EmptyList.add(10).add(20).add(30)
  println("newList :: " + newList)
  println("anotherList :: " + anotherList)
}

abstract class MyList {
  def head: Int
  def tail: MyList
  def isEmpty: Boolean
  def add(element: Int): MyList
  def printElements: String
  override def toString: String = "[" + printElements + "]"
}

object EmptyList extends MyList {
  override def head: Int = throw new NoSuchElementException

  override def tail: MyList = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def add(element: Int): MyList = new IntList(element, EmptyList)

  override def printElements: String = ""
}

class IntList(h: Int, t: MyList) extends MyList {
  override def head: Int = h

  override def tail: MyList = t

  override def isEmpty: Boolean = false

  override def add(element: Int): MyList = new IntList(element, this)

  override def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements
}