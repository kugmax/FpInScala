package com.kugmax.learn.scala.errorhndling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(x) => Right(f(x))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(x) => f(x)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(x) => Right(x)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      a <- this
      bb <- b
    } yield f(a, bb)
  }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Runner {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {

    es.foldRight[ Either[E, List[B]] ] (Right(Nil:List[B])) ( (e, acc) => acc.map2( f(e) )  ((l, b) => l.appended(b))  )
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
    es.foldRight[Either[E, List[A]]] ( Right(Nil:List[A]) ) ( (e, acc) => acc.map2(e)  ((l, b) => l.appended(b))  )
  }

  def traverse_2[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def traverse_1[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence_1[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(x => x)


  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }


  def main(args: Array[String]): Unit = {
    System.out.println(
      sequence(
//        List(Right(1), Right(2), Left("Broken"))
        List(Right(1), Right(2))
      )
    )

    System.out.println(
      traverse(List(1, 2, 3)) ( (x) => Right(x +1) )
    )

  }
}