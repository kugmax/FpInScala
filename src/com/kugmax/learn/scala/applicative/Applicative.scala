package com.kugmax.learn.scala.applicative

import java.util.Date

import com.kugmax.learn.scala.functor.Functor
import com.kugmax.learn.scala.monad.Monad

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
}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object Applicative {

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

