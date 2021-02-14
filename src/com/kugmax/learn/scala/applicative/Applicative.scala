package com.kugmax.learn.scala.applicative

import java.util.Date

import com.kugmax.learn.scala.datastructure.Tree
import com.kugmax.learn.scala.functor.Functor
import com.kugmax.learn.scala.monad.Monad
import com.kugmax.learn.scala.monoids.Monoid
import com.kugmax.learn.scala.monoids.Monoid.Foldable
import com.kugmax.learn.scala.purlystate.State
import com.sun.org.apache.bcel.internal.Const

import scala.reflect.macros.ParseException

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

  def assoc[A,B,C](p: (A,(B,C))): ((A,B), C) =
    p match { case (a, (b, c)) => ((a,b), c) }

  def productF[I,O,I2,O2](f: I => O, g: I2 => O2): (I,I2) => (O,O2) =
    (i,i2) => (f(i), g(i2))

  def product[G[_]](g: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this

    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), g.unit(a))
      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        (self.map2(fa._1, fb._1)(f),  g.map2(fa._2, fb._2)(f))
    }
  }

  def compose[G[_]](g: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this

    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)(g.map2(_,_)(f))

      override def unit[A](a: => A): F[G[A]] = self.unit(g.unit((a)))
    }
  }

  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    def map2[A,B,C](a: Stream[A], b: Stream[B])(
      f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
        case Left(a) => Left(a)
        case Right(a) => f(a)
      }
    }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    (ofa foldLeft unit(Map.empty[K,V])) { case (acc, (k, fv)) => map2(acc, fv)((m, v) => m + (k -> v))}
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[M[_]:Applicative,A,B](fa: F[A])(f: A => M[B]): M[F[B]] =
    sequence(map(fa)(f))

  def sequence[M[_]:Applicative,A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Applicative.Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def zipWithIndex_[A](ta: F[A]): F[(A,Int)] =
    traverseS(ta)((a: A) => (for {
      i <- State.get[Int]
      _ <- State.set(i + 1)
    } yield (a, i))).run(0)._1

  def toList_[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => (for {
      as <- State.get[List[A]] // Get the current state, the accumulated list.
      _  <- State.set(a :: as) // Add the current element and set the new list as the new state.
    } yield ())).run(Nil)._2.reverse

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- State.get[S]
      (b, s2) = f(a, s1)
      _  <- State.set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  def zip[A,B](fa: F[A], fb: F[B]): F[(A, B)] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    })._1

  def zipL[A,B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    })._1

  def zipR[A,B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    (mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    })._1

  def fuse[M[_],N[_],A,B](fa: F[A])(f: A => M[B], g: A => N[B])
                         (implicit M: Applicative[M], N: Applicative[N]): (M[F[B]], N[F[B]]) =
    traverse[({type f[x] = (M[x], N[x])})#f, A, B](fa)(a => (f(a), g(a)))(M product N)

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]) =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }
}


object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[M[_],A,B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[M[_],A,B](oa: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
      oa match {
        case Some(a) => M.map(f(a))(Some(_))
        case None    => M.unit(None)
      }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[M[_],A,B](ta: Tree[A])(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] =
      M.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }
}


case class Tree[+A](head: A, tail: List[Tree[A]])


sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object Applicative {

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({type f[x] = Applicative.this.Const[M, x]})#f] =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      def map2[A,B,C](m1: M, m2: M)(f: (A,B) => C): M = M.op(m1,m2)
    }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {

      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
        case (Success(a), Success(b) ) => unit(f(a, b))
        case (Failure(ea, errA), Failure(eb, errB)) => Failure(ea, errA.appended(eb).appendedAll(errB))
        case (Failure(e, err), _) => Failure(e, err)
        case (_, Failure(e, err)) => Failure(e, err)
      }
    }

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    } catch {
      case _: ParseException => Failure("Birthdate must be in the form yyyy-MM-dd")
    }


  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

//  def validWebForm(name: String,
//                   birthdate: String,
//                   phone: String): Validation[String, WebForm] =
//    map3(
//      validName(name),
//      validBirthdate(birthdate),
//      validPhone(phone))(WebForm(_,_,_))

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

