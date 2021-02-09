package com.kugmax.learn.scala.monoids

import com.kugmax.learn.scala.concurrent.Par
import com.kugmax.learn.scala.concurrent.Par.{Par, map2}
import com.kugmax.learn.scala.datastructure.Tree
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

  def isOrdered(ints: IndexedSeq[Int]): Boolean = {
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
        (o1, o2) match {
          // The ranges should not overlap if the sequence is ordered.
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      val zero = None
    }
    // The empty sequence is ordered, and each element by itself is ordered.
    foldMapV(ints, mon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(l), Stub(r)) => Stub(l + r)
      case (Part(ll, lw, lr), Stub(r)) => Part(ll, lw, lr+r)
      case (Stub(l), Part(rl, rw, rr)) => Part(l+rl, rw, rr)
      case (Part(ll, lw, lr), Part(rl, rw, rr)) => Part(ll, lw + (if ((lr + rl).isEmpty) 0 else 1) + rw, rr)
    }
    override def zero: WC = Stub("")
  }

  def count(s: String): Int = {

    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    def unstub(s: String) = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  trait Foldable[F[_]] {
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
    def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)
  }

  val foldableList = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as.foldRight(mb.zero)((a,b) => mb.op(f(a), b))
  }

  val foldableIndexedSeq = new Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = as.foldRight(mb.zero)((a,b) => mb.op(f(a), b))
  }

  val foldableStream = new Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = as.foldRight(mb.zero)((a,b) => mb.op(f(a), b))
  }

  val foldableTree = new Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = ???

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = ???

    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = ???
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

//    println(isOrdered(IndexedSeq(1, 2, 3)))
//    println(isOrdered(IndexedSeq(3, 2, 1)))
//    println(isOrdered(IndexedSeq(2, 1, 3)))

    run(monoidLaws(wcMonoid, Gen.unit("lorem ipsum do")))
//    run(monoidLaws(wcMonoid, Gen.unit("lor sit amet, ")))
  }

 }
