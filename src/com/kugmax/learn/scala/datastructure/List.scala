package com.kugmax.learn.scala.datastructure

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_+_)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_*_)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight2(a1, a2)( Cons(_, _) )

  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(a1, a2)( (b: List[A], a: A) => Cons(a, b) )

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      def loop(count: Int, lInner: List[A]): List[A] = {
        if (count == 0) lInner
        else loop(count - 1, tail(lInner))
      }
      loop(n, l)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x: A, xs: List[A]) =>
      if (f(x)) dropWhile(xs, f)
      else xs
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) =>
      xs match {
        case Cons(_, ys) => Cons(x, xs)
        case Cons(_, Nil) => Cons(x, Nil)
      }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0) ((_, b) =>  b +1)

  def length2[A](l: List[A]): Int = foldLeft(l, 0) ((b, a) =>  b +1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) =>
        f(x, foldRight(xs, z)(f))
  }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def reverse[A](l : List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Cons(x, Nil)
//    case _  => foldLeft(l, Nil:List[A] )( (b: List[A], a: A ) => Cons(a, b)  )
    case _  => foldLeft2(l, Nil:List[A] )( (b: List[A], a: A ) => Cons(a, b)  )
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = ???
}
