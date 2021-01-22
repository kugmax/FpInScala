package com.kugmax.learn.scala.testing

import com.kugmax.learn.scala.purlystate.SimpleRNG.{nonNegativeInt}
import com.kugmax.learn.scala.purlystate.{RNG, SimpleRNG, State}

case class Gen[A](sample: State[RNG,A]) {
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(SimpleRNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(13)

    println(choose(1, 94345).sample.run(rng) )
    println(unit(99).sample.run(rng) )
    println(boolean.sample.run(rng) )

    println(listOfN(7, choose(1, 9) ).sample.run(rng) )
  }
}
