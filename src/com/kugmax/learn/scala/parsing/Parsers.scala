package com.kugmax.learn.scala.parsing
import com.kugmax.learn.scala.testing.Prop._
import com.kugmax.learn.scala.testing._

import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

//  primitives:
  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  def slice[A](p: Parser[A]): Parser[String]
//  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  def succeed[A](a: A): Parser[A]
  //  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]
  def attempt[A](p: Parser[A]): Parser[A]


//  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
//  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))

  def map2[A,B,C](p1: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    product(p1, p2).map(ab => f(ab._1, ab._2))

  def _map2[A,B,C](p1: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    p1.flatMap(a => p2.map(b => f(a, b)))

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] =
    p.flatMap(a => p2.map(b => (a, b)))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def succeed(a: A): Parser[A] = self.succeed(a)
    def slice: Parser[String] = self.slice(p)
    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  case class ParseError(stack: List[(Location,String)])

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
    lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }
  }
  def errorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): String

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

//    run(succeed(a))(s) == Right(a)
//    map(p)(a => a) == p

//    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
//      forAll(inputs ** Gen.string) { case (input, msg) =>
//        run(label(msg)(p))(input) match {
//          case Left(e) => errorMessage(e) == msg
//          case _ => true
//        }
//      }
  }

}
