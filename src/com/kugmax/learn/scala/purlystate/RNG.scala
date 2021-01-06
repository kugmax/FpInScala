package com.kugmax.learn.scala.purlystate

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (value, nextRng) = rng.nextInt
    if (value < 0) (-(value + 1), nextRng) else (value, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (value, nextRng) = rng.nextInt
    (value / (Int.MaxValue.toDouble + 1), nextRng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (v1, nextRng) = rng.nextInt
    val (v2, nextRng1) = double(nextRng)

    ( (v1, v2), nextRng1)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ( (i, d), nextRng) = intDouble(rng)
    ((d, i), nextRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, nextRng1) = double(rng)
    val (d2, nextRng2) = double(nextRng1)
    val (d3, nextRng3) = double(nextRng2)

    ((d1, d2, d3), nextRng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    def loop(n:Int, r: RNG, l: List[Int]): (List[Int], RNG) = {
      if (n <= 0) {
        return (l, r)
      }
      val (v, nextR) = r.nextInt
      loop(n-1, nextR, v :: l)
    }

    loop(count, rng, Nil)
  }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

//  def double2(rng: RNG): (Double, RNG) = {
//
//  }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(13)
    val (value, nRng) = rng.nextInt

//    println(value, nRng)

    val (posVal1, nRng1) = ints(7)(nRng)
    println(posVal1, nRng1)

  }
}