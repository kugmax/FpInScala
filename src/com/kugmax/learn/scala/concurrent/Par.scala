package com.kugmax.learn.scala.concurrent

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map2WithTimeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C)(l: Long, t: TimeUnit): Par[C] =
    (es: ExecutorService) => {

      val start = System.currentTimeMillis()
      val aValue = a(es).get(l, t)
      val end = System.currentTimeMillis()

      val delta = end - start

      val leftTimeout = t.convert(delta, TimeUnit.MILLISECONDS)

      val bValue = b(es).get(leftTimeout, t)
      UnitFuture(f(aValue, bValue))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

//  def asyncF[A,B](f: A => B): A => Par[B] = _ => lazyUnit(f)
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]] (unit(List())) ((x, acc) => map2(x, acc)(_ :: _))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = ???

}
