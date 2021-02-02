package com.kugmax.learn.scala.testing

import java.util.concurrent.{ExecutorService, Executors}

import com.kugmax.learn.scala.concurrent.Par
import com.kugmax.learn.scala.concurrent.Par.Par
import com.kugmax.learn.scala.purlystate.SimpleRNG.{double, nonNegativeInt, unit}
import com.kugmax.learn.scala.purlystate.{RNG, SimpleRNG, State}
import com.kugmax.learn.scala.testing.Gen.{choose, unit, weighted}
import com.kugmax.learn.scala.testing.Prop.{check, checkPar, equal, forAll, run}

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

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
    def &&(p: Prop): Prop = Prop(
      (size, t, rng) =>
        run(size, t, rng) match {
          case Passed | Proved => p.run(size, t, rng)
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

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = SimpleRNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

//  def check(p: => Boolean): Prop = {
//    lazy val result = p
//    forAll(Gen.unit(()))(_ => result)
//  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  val S: Gen[ExecutorService] = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll( S ** g ) { case s ** a => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

}

object ** {
  def unapply[A,B](p: (A,B)) = Some(p)
}

case class Gen[A](sample: State[RNG,A]) {

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

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

  def map[B](f: A => B): SGen[B] =
    SGen { g(_) map f }
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

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(SimpleRNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen( n => g.listOfN(n) )

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen( _ => g.listOfN(1) )

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g map (i => (s => i))

  def main(args: Array[String]): Unit = {
//    val rng = SimpleRNG(13)

    //    println(choose(0.5, 0.9).sample.run(rng) )
    //    println(unit(99).sample.run(rng) )
    //    println(boolean.sample.run(rng) )
    //
    //    println(listOfN(7, choose(1, 9) ).sample.run(rng) )

    //    println(boolean.flatMap( b => unit(99) ).sample.run(rng)  )
    //    println(boolean._flatMap( b => unit(99) ).sample.run(rng)  )

    //    println(choose(1, 9).listOfN(unit(2)).sample.run(rng) )

    //    println(union(unit(1), unit(2) ).sample.run(rng) )

    //    val result = Prop.forAll(choose(1, 10))(v => true) && Prop.forAll(unit(3))(v => v == 3)
    //    val result = Prop.forAll(choose(1, 10))(v => false) || Prop.forAll(unit(3))(v => v != 3)
    //    println( result.run(10, 4, rng) )

    val smallInt = Gen.choose(-10, 10)
//    val maxProp = Prop.forAll(listOf1(smallInt)) { ns =>
//      val max = ns.max
//      !ns.exists(_ > max)
//    }
//    Prop.run(maxProp)

//    val sortedProp = forAll(listOf(smallInt)) { l =>
//      val ls = l.sorted
//      l.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a,b) => a > b }
//    }
//
//    Prop.run(sortedProp)

    val ES: ExecutorService = Executors.newCachedThreadPool
//    val p2 = Prop.check {
//      val p = Par.map(Par.unit(1))(_ + 1)
//      val p2 = Par.unit(2)
//      p(ES).get == p2(ES).get
//    }
//
//    run(p2)

//    val p3 = check {
//      equal(
//        Par.map(Par.unit(1))(_ + 1),
//        Par.unit(2)
//      )(ES).get
//    }
//    run(p3)

    val p2 = checkPar {
      equal (
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )
    }
    run(p2)

  }
}
