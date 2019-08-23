package com.kugmax.learn.scala

object CheckSortedRunner {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) : Boolean = {
    if (as.length <= 1) return true

    def loop(n: Int): Boolean = {
      if (n == as.length) true
      else if ( !ordered(as(n - 1), as(n) ) ) false
      else loop(n + 1)
    }

    loop(1)
  }


  def main(args: Array[String]): Unit = {

    val comparator = (v1 : Int, v2 : Int) => v1 <= v2

    System.out.println("true = " +  isSorted[Int]( Array(1, 2, 3),  comparator ))
    System.out.println("false = " +  isSorted[Int]( Array(2, 1, 3),  comparator ))
  }
}