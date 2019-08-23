package com.kugmax.learn.scala

object Runner {

  def fibonacci(n: Int): Int = {
    def loop(nCur: Int,  prev1: Int, prev2: Int) :Int = {
      if (n == 0) 0
      else if (n <= nCur) prev2
      else loop(nCur + 1, prev2, prev1 + prev2)
    }

    loop(1, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    System.out.println("Hello World !!!")
    System.out.println("0 " + fibonacci(0))
    System.out.println("1 " + fibonacci(1))
    System.out.println("2 " + fibonacci(2))
    System.out.println("3 " + fibonacci(3))
    System.out.println("4 " + fibonacci(4))
    System.out.println("5 " + fibonacci(5))
    System.out.println("6 " + fibonacci(6))
  }
}
