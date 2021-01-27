package com.kugmax.learn.scala.testing

import com.kugmax.learn.scala.purlystate.SimpleRNG.{double, nonNegativeInt, unit}
import com.kugmax.learn.scala.purlystate.{RNG, SimpleRNG, State}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
    def &&(p: Prop): Prop = Prop(
      (size, t, rng) =>
        run(size, t, rng) match {
          case Passed => p.run(size, t, rng)
          case x => x
        }
      )

    def ||(p: Prop): Prop = Prop(
      (size, t, rng) =>
        run(size, t, rng) match {
          case Falsified(msg, _) => p.tag(msg).run(size, t, rng)
          case x => x
        }
      )

    def tag(msg: String): Prop = Prop {
      (size, n,rng) => run(size, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (size,n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))

      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

}

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

  def listOfN(n: Int): Gen[List[A]] = Gen.listOfN(n, this)

  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)
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

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen( n => g.listOfN(n) )

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(13)

//    println(choose(0.5, 0.9).sample.run(rng) )
//    println(unit(99).sample.run(rng) )
//    println(boolean.sample.run(rng) )
//
//    println(listOfN(7, choose(1, 9) ).sample.run(rng) )

//    println(boolean.flatMap( b => unit(99) ).sample.run(rng)  )
//    println(boolean._flatMap( b => unit(99) ).sample.run(rng)  )

//    println(choose(1, 9).listOfN(unit(2)).sample.run(rng) )

//    println(union(unit(1), unit(2) ).sample.run(rng) )

    val result = Prop.forAll(choose(1, 10))(v => true) && Prop.forAll(unit(3))(v => v == 3)
//    val result = Prop.forAll(choose(1, 10))(v => false) || Prop.forAll(unit(3))(v => v != 3)


    println( result.run(10, 4, rng) )
  }
}
