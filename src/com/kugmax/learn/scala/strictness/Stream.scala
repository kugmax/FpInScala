package com.kugmax.learn.scala.strictness

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => List(h()).appendedAll(t().toList)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (n <= 0) Empty
      else Cons(h, () => t().take(n-1))
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (!p(h())) Empty
      else Cons(h, () => t().takeWhile(p))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (n == 0) Cons(h, t)
      else t().drop(n-1)
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def exists2(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
  }

  def forAll(p: A => Boolean): Boolean = exists2(p)

  def takeWhile2(p: A => Boolean): Stream[A] = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


  def main(vars: Array[String]): Unit = {
    System.out.println(
      Stream(1,2,3).toList
    )

    System.out.println(
      Stream(1,2,3).drop(1).toList
    )

    System.out.println(
      Stream(1,2,3).take(2).toList
    )

    System.out.println(
      Stream(1,2,3).takeWhile( v => v <= 2 ).toList
    )
  }
}