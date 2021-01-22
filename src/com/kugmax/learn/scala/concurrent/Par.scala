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

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

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

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = l map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es => {
      val chosen = run(es)(n).get()
      choices(chosen)(es)
    }
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    choiceN(map(cond)(c => if (c) 0 else 1 ) )(List(t, f))
  }

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = {
    es => {
      val chosen = run(es)(key).get()
      choices(chosen)(es)
    }
  }

  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es => {
      val chosen = run(es)(pa).get()
      choices(chosen)(es)
    }
  }

  def flatMap[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)

  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))

  def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(run(es)(a).get())

}
