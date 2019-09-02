package com.kugmax.learn.scala.errorhndling

import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob()
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if f(v) => this
    case _ => None
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(
      m => mean(xs.map(x => math.pow(x - m, 2)))
    )
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) {
    case (None, _) => None
    case (_, None) => None
    case (Some(v1), Some(v2)) => Some(f(v1, v2))
  }


  def map3[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa =>
      b map (bb =>
        f(aa, bb)))

  def map4[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = {

    try Some(a.map( a => a.getOrElse(throw new Exception("Can't"))))
    catch {case e: Exception => None}

  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil)) ((x, acc) => map2(x, acc)(_ :: _))
  }


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil)) ((x, acc) => map2(f(x), acc)(_ :: _) )
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

