package com.kugmax.learn.scala.strictness

import com.kugmax.learn.scala.strictness.Stream.unfold

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

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] = unfold(this) {
    case Cons(h, t) => if (n <= 0) None else Some(h(), t().takeViaUnfold(n-1))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) => if (!p(h())) None else Some(h(), t().takeWhileViaUnfold(p))
    case _ => None
  }

  def zipWith[B, C](l2:Stream[B]) (f: (A,B) => C ): Stream[C] = unfold(this, l2) {
    case (Cons(h, t), Cons(h2, t2)) => Some( f(h(), h2()), (t(), t2()) )
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold(this, s2) {
    case (Cons(h, t), Cons(h2, t2)) => Some( (Some(h()), Some(h2())), (t(), t2()) )
    case (Empty, Cons(h2, t2)) => Some( (None, Some(h2())), (Empty, t2()) )
    case (Cons(h, t), Empty) => Some( (Some(h()), None), (t(), Empty) )
    case _ => None
  }

  def startsWith[A](s2: Stream[A]): Boolean = (this, s2) match {
    case (Cons(h, t), Cons(h2, t2)) => if (h() != h2()) false else t().startsWith(t2())
    case _ => true
  }

  def startsWith2[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h, t) => Some(Cons(h, t), t() )
    case _ => None
  } append Stream(Empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Cons(() => b2, () => p1._2))
    })._2

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)
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

  def from2(n: Int): Stream[Int] = unfold(cons(n, Empty)) (s => Some(s.headOption.get ,(cons( s.headOption.get + 1, s )) ))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s,s+1)))

  def fibs2: Stream[Int] = {
    unfold(cons((0, 1), Empty)) (s =>
      Some(s.headOption.get._1,
        cons(
          (s.headOption.get._2, s.headOption.get._1 + s.headOption.get._2),
          s
        )
      ))
  }

  def constant2[A](a: A): Stream[A] = unfold(cons(a, Empty)) (s => Some(a, s) )

  val ones2: Stream[Int] = unfold(cons(1, Empty)) (s => Some(1, s) )


  def main(vars: Array[String]): Unit = {
    System.out.println(
      Stream(1,2,3).scanRight(0)(_ + _).toList
    )

  }
}