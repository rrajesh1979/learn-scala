package com.learn

object LearnVariance extends App {

  class Animal
  class Lion extends Animal
  class Fish extends Animal

  //Covariance
  class CVList[+A]

  val cvAnimalList: CVList[Animal] = new CVList[Lion]

}
