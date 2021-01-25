package com.kugmax.learn.scala.testing

import com.kugmax.learn.scala.purlystate.SimpleRNG.{boolean, double, nonNegativeInt}
import com.kugmax.learn.scala.purlystate.{RNG, SimpleRNG, State}

case class Gen[A](sample: State[RNG,A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(State(
    s => {
      val (a, s1) = sample.run(s)
      f(a).sample.run(s1)
    }
  ))

  def _flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen(State.sequence(List.fill(n)(sample))))

}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def choose(start: Double, stopExclusive: Double): Gen[Double] =
    Gen(State(double).map(n => start + n % (stopExclusive-start)))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(SimpleRNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if(b) g1 else g2 )

  def _weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(SimpleRNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(13)

    println(choose(0.5, 0.9).sample.run(rng) )
//    println(unit(99).sample.run(rng) )
//    println(boolean.sample.run(rng) )
//
//    println(listOfN(7, choose(1, 9) ).sample.run(rng) )

//    println(boolean.flatMap( b => unit(99) ).sample.run(rng)  )
//    println(boolean._flatMap( b => unit(99) ).sample.run(rng)  )

//    println(choose(1, 9).listOfN(unit(2)).sample.run(rng) )

//    println(union(unit(1), unit(2) ).sample.run(rng) )
  }
}
