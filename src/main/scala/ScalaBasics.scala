import scala.language.postfixOps

object ScalaBasics extends App {
  class Person(val firstName: String, lastName: String, age: Int) {
    val fullName: String = firstName + " " + lastName
    def getName: String = fullName
    def greeting: String = s"Hello Scala from $fullName"
    def likes(movie: String): String = s"$fullName likes $movie"

    def !!(progLanguage: String): String =
      s"$fullName wonders how can $progLanguage be so cool!"

    def unary_! : String = s"This is from a unary operator :: {$age}"

    def apply(num: Int): String =
      s"Apply operator :: $num :: $fullName"
  }

  val john: Person = new Person("John", "Doe", 50)
  println(john.firstName)
  println(john.greeting)

  println(john likes "Harry Potter") //infix notation

  println(!john)  //custom unary operator

  println(john(100)) //apply method
  //apply method can be used for builder method pattern
}
