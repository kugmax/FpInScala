package com.kugmax.learn.scala.datastructure

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def sizeMy[A](t: Tree[A]): Int = {

    def loop(t: Tree[A], acc: Int): Int = t match {
      case Leaf(_) => acc + 1
      case Branch(l, r) =>
        val accI = loop(l, acc + 1)
        loop(r, accI + 1)
    }

    loop(t, 0)
  }

  def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = {
    case Leaf(v) => v
    case Branch(l:Tree[Int], r:Tree[Int]) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def mapMy[A,B](t: Tree[A])(f: A => B) : Tree[B] = {
    case Leaf(v) => Leaf( f(v) )
    case Branch(l:Tree[A], _) => Branch(mapMy(l)(f), _)
    case Branch(_,r:Tree[A]) => Branch(_, mapMy(r)(f))
  }

//  def map[A,B](t: Tree[A])(f: A => B) : Tree[B] = {
//    case Leaf(v) => Leaf( f(v) )
//    case Branch(l, r) => Branch( map(l)(t), map(r)(t) )
//  }


  def fold[A,B](t: Tree[A], z:B)(f: (A,B) => B)(g:(B, B) => B) : Tree[B] = {
    case Leaf(v) => f(v, z)
    case Branch(l:Tree[A],r:Tree[A]) => g(fold(l, z)(f)(g), fold(r, z)(f)(g) )
//    case Branch(l:Tree[A], _) => fold(l, z)(f)
//    case Branch(_, r:Tree[A]) => fold(r, z)(f)

  }
}