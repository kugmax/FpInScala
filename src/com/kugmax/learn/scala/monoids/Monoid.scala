package com.kugmax.learn.scala.monoids

import com.kugmax.learn.scala.concurrent.Par
import com.kugmax.learn.scala.concurrent.Par.{Par, map2}
import com.kugmax.learn.scala.testing.Gen
import com.kugmax.learn.scala.testing.Prop.{Prop, checkPar, equal, forAll, run}

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1  * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
//    op(op(x, y), z) = op(x, op(y, z))
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)
    override def zero: A => A = (a: A) => a
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    forAll(gen.listOfN(3))(l => m.op(l(0), m.op(l(1), l(2))) == m.op(m.op(l(0), l(1)), l(2) ))
    forAll(gen.listOfN(3))(l => m.op(l(0), m.zero ) == l(0) )
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.isEmpty)
      m.zero
    else if (as.length == 1)
      f(as(0))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
      override def op(a: Par[A], b: Par[A]): Par[A] = map2(a, b)(m.op)
      override def zero: Par[A] = Par.unit(m.zero)
  }

//  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
//    Par.parMap(v)(f).flatMap { bs =>
//      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
//    }

  def isOrdering(v: IndexedSeq[Int]): Boolean = {

    val orderingMonoid = new Monoid[ (Int, Int) ] {
      override def op(a1: (Int, Int), a2: (Int, Int)): (Int, Int) = ???

      override def zero: (Int, Int) = (0, 0)
    }

    foldMapV(v, orderingMonoid)( a => true )

  }

  def main(args: Array[String]): Unit = {
//    run(monoidLaws(intAddition, Gen.choose(0, 6)))
//    run(monoidLaws(stringMonoid, Gen.choose(0, 6).map(_ => toString)))

//    val words = List("Hic", "Est", "Index")
//    val r = words.foldRight(stringMonoid.zero)(stringMonoid.op)
//    println(r)
//
//    val l = words.foldLeft(stringMonoid.zero)(stringMonoid.op)
//    println(l)

    println(isOrdering(IndexedSeq(1, 2, 3)))
    println(isOrdering(IndexedSeq(3, 2, 1)))
    println(isOrdering(IndexedSeq(2, 1, 3)))
  }

 }
