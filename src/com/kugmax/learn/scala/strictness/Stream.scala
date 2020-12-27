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

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => foldRight[Stream[A]](Empty)( (a, b) => if (!p(h())) Empty else Cons(h, () => t().takeWhile2(p)) )
  }

  def headOption2: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => foldRight[Option[A]](Some(h()))( (a, _) => Some(a) )
  }

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)( (h, acc) => Cons( () => f(h) , () => acc) )

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)( (h, acc) => if (f(h)) Cons(() => h, () => acc) else acc )

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty)( (h, acc) => f(h).append( acc ) )

  def append[B >: A](a2: Stream[B]): Stream[B] =
    foldRight[Stream[B]](a2)( (h, acc) => Cons(() => h,() => acc) )

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
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

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val stream: Stream[A] = Cons(() => a, () => stream)
    stream
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def fibs: Stream[Int] = {
    def loop(n: Int, n2: Int): Stream[Int] = {
      Cons(() => n, () => loop(n2, n + n2))
    }
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h,s)) => cons(h, unfold(s)(f))
    case None => empty
  }

//  Write fibs , from , constant , and ones in terms of unfold .

  def fibs2: Stream[Int] = {
//    unfold()
    null
  }

  def main(vars: Array[String]): Unit = {
    System.out.println(
    )
  }
}