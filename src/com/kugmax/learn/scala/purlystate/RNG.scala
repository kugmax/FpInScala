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

  def double2: Rand[Double] = map(double)(i => i)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, na) = ra(rng)
      val (b, nb) = rb(na)
      (f(a, b), nb)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (v1, rng1) = f(rng)
      g(v1)(rng1)
    }
  }

  def nonNegativeLessThan_usingFlatMap(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(
      i =>
      {
        val mod = i % n
        if (i + (n-1) - mod >= 0)
          unit(mod)
        else nonNegativeLessThan(n)
      }
    )
  }

  def map_usingFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(i => unit(f(i)))
  def map2_usingFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

  def _map[S,A,B](a: S => (A,S))(f: A => B): S => (B,S) = ???

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(13)
    val (value, nRng) = nonNegativeLessThan_usingFlatMap(1)(rng)

    println(value, nRng)

  }
}