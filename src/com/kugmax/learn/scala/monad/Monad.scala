package com.kugmax.learn.scala.monad

import com.kugmax.learn.scala.concurrent.Par
import com.kugmax.learn.scala.concurrent.Par.Par
import com.kugmax.learn.scala.functor.Functor
import com.kugmax.learn.scala.parsing.Parsers
import com.kugmax.learn.scala.purlystate.State
import com.kugmax.learn.scala.testing.Gen

/**
 * A monad is an implementation of one of the minimal sets of monadic
 * combinators, satisfying the laws of associativity and identity.
 *
 * - unit and flatMap
 * - unit and compose
 * - unit , map , and join
 */

trait Monad[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(fa => fa)

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight( unit(List[B]()) )( (a, acc) => map2(f(a), acc)( _::_ ) )

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

//  def _flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
//    compose((_:Unit) => ma, f)(())

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]()))((x,y) =>
      compose(f, (b: Boolean) => if (b) map2(unit(x),y)(_ :: _) else y)(x))

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(a => a)

  def _compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))

}

object Monad {

  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

//  val parserMonad: Monad[Parsers] = new Monad[Parsers] {
//    override def unit[A](a: => A): Parsers[A] = ???
//    override def flatMap[A, B](ma: Parsers[A])(f: A => Parsers[B]): Parsers[B] = ???
//  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option.apply(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream.apply(a)
    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

//  val stateMonad: Monad[State] = new Monad[State] {
//    override def unit[A](a: => A): State[A, B] = ???
//
//    override def flatMap[A, B](ma: State[A])(f: A => State[B]): State[B] = ???
//  }

  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  type IntState[A] = State[Int, A]

  object IntStateMonad extends Monad[IntState] {
    def unit[A](a: => A): IntState[A] = State(s => (a, s))
    def flatMap[A,B](st: IntState[A])(f: A => IntState[B]): IntState[B] = st flatMap f
  }

  object _IntStateMonad extends Monad[({type IntState[A] = State[Int, A]})#IntState] {
    override def unit[A](a: => A): State[Int, A] = ???

    override def flatMap[A, B](ma: State[Int, A])(f: A => State[Int, B]): State[Int, B] = ???
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S,A] = State(s => (a, s))
    def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] = st flatMap f
  }

//  val F = stateMonad[Int]
//  def zipWithIndex[A](as: List[A]): List[(Int,A)] =
//    as.foldLeft(F.unit(List[(Int, A)]()))((acc,a) => for {
//      xs <- acc
//      n <- getState
//      _ <- setState(n + 1)
//    } yield (n, a) :: xs).run(0)._1.reverse


  def main(s: Array[String]): Unit = {
    println(
      Id("Hello, ") flatMap (a =>Id("monad!") flatMap (b =>Id(a + b)))
    )

    val result =
      for {
        a <- Id("Hello, ")
        b <- Id("monad!")
    } yield a + b

    println(result)
  }
}