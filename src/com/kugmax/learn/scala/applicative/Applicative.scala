package com.kugmax.learn.scala.applicative

import com.kugmax.learn.scala.functor.Functor

trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(a => a)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((a, b) => a(b))

  def _map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def _map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(_map(fa)(f.curried))(fb)

  def _map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val curry1: F[B => C => D] = _map(fa)(f.curried)
    val app1: F[C => D] = apply(curry1)(fb)
    apply(app1)(fc)
  }

  def _map3[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val curry1: F[B => C => D => E] = _map(fa)(f.curried)
    val app1: F[C => D => E] = apply(curry1)(fb)
    val app2: F[D => E] = apply(app1)(fc)
    apply(app2)(fd)
  }

  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    def map2[A,B,C](a: Stream[A], b: Stream[B])(
      f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def sequence[A](a: List[Stream[A]]): Stream[List[A]] = ???
}

trait MonadA[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
}

