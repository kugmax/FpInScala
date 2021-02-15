package com.kugmax.learn.scala.effects

import com.kugmax.learn.scala.concurrent.Par
import com.kugmax.learn.scala.concurrent.Par.Par
import com.kugmax.learn.scala.monad.Monad

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object IO0 {

  trait IO { self =>
    def run: Unit
    def ++(io: IO): IO = new IO {
      def run = { self.run; io.run }
    }
  }
  object IO {
    def empty: IO = new IO { def run = () }
  }

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  def converter: Unit = {
    println("Enter a temperature in degrees Fahrenheit: ")
    val d = readLine.toDouble
    println(fahrenheitToCelsius(d))
  }

}

object IO1 {

  sealed trait IO[A] {
    self =>
    def run: A

    def map[B](f: A => B): IO[B] =
      new IO[B] {
        def run = f(self.run)
      }

    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO[B] {
        def run = f(self.run).run
      }
  }

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] {
      def run = a
    }

    def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

    def apply[A](a: => A): IO[A] = unit(a)

    def ref[A](a: A): IO[IORef[A]] = IO {
      new IORef(a)
    }

    sealed class IORef[A](var value: A) {
      def set(a: A): IO[A] = IO {
        value = a; a
      }

      def get: IO[A] = IO {
        value
      }

      def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
    }

  }

  def ReadLine: IO[String] = IO {
    readLine
  }

  def PrintLine(msg: String): IO[Unit] = IO {
    println(msg)
  }

  import IO0.fahrenheitToCelsius

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  import IO._


  object IO2a {

    sealed trait IO[A] {
      def flatMap[B](f: A => IO[B]): IO[B] =
        FlatMap(this, f)

      def map[B](f: A => B): IO[B] =
        flatMap(f andThen (Return(_)))
    }

    case class Return[A](a: A) extends IO[A]

    case class Suspend[A](resume: () => A) extends IO[A]

    case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

    object IO extends Monad[IO] { // Notice that none of these operations DO anything
      def unit[A](a: => A): IO[A] = Return(a)

      def flatMap[A, B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f

      def suspend[A](a: => IO[A]) =
        Suspend(() => ()).flatMap { _ => a }
    }

    def printLine(s: String): IO[Unit] =
      Suspend(() => Return(println(s)))

    val actions: Stream[IO[Unit]] =
      Stream.fill(100000)(printLine("Still going..."))

    val composite: IO[Unit] =
      actions.foldLeft(IO.unit(())) { (acc, a) => acc flatMap { _ => a } }

    @annotation.tailrec def run[A](io: IO[A]): A = io match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
    }
  }

  object IO2b {

    sealed trait TailRec[A] {
      def flatMap[B](f: A => TailRec[B]): TailRec[B] =
        FlatMap(this, f)

      def map[B](f: A => B): TailRec[B] =
        flatMap(f andThen (Return(_)))
    }

    case class Return[A](a: A) extends TailRec[A]

    case class Suspend[A](resume: () => A) extends TailRec[A]

    case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

    object TailRec extends Monad[TailRec] {
      def unit[A](a: => A): TailRec[A] = Return(a)

      def flatMap[A, B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
        a flatMap f

      def suspend[A](a: => TailRec[A]) =
        Suspend(() => ()).flatMap { _ => a }
    }

    @annotation.tailrec def run[A](t: TailRec[A]): A = t match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
    }
  }

  object IO2bTests {

    import IO2b._

    val f: Int => TailRec[Int] = (i: Int) => Return(i)

    val g: Int => TailRec[Int] =
      List.fill(10000)(f).foldLeft(f) {
        (a: Function1[Int, TailRec[Int]],
         b: Function1[Int, TailRec[Int]]) => {
          (x: Int) => TailRec.suspend(a(x).flatMap(b))
        }
      }

    def main(args: Array[String]): Unit = {
      val gFortyTwo = g(42)
      println("g(42) = " + gFortyTwo)
      println("run(g(42)) = " + run(gFortyTwo))
    }
  }

  object IO2c {

    sealed trait Async[A] { // will rename this type to `Async`
      def flatMap[B](f: A => Async[B]): Async[B] =
        FlatMap(this, f)

      def map[B](f: A => B): Async[B] =
        flatMap(f andThen (Return(_)))
    }

    case class Return[A](a: A) extends Async[A]

    case class Suspend[A](resume: Par[A]) extends Async[A] // notice this is a `Par`
    case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

    object Async extends Monad[Async] {
      def unit[A](a: => A): Async[A] = Return(a)

      def flatMap[A, B](a: Async[A])(f: A => Async[B]): Async[B] = a flatMap f
    }

    // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
    @annotation.tailrec def step[A](async: Async[A]): Async[A] = async match {
      case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
      case FlatMap(Return(x), f) => step(f(x))
      case _ => async
    }

    def run[A](async: Async[A]): Par[A] = step(async) match {
      case Return(a) => Par.unit(a)
      case Suspend(r) => r
      case FlatMap(x, f) => x match {
        case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
        case _ => sys.error("Impossible, since `step` eliminates these cases")
      }
    }
  }


  object IO3 {


    sealed trait Free[F[_], A] {
      def flatMap[B](f: A => Free[F, B]): Free[F, B] =
        FlatMap(this, f)

      def map[B](f: A => B): Free[F, B] =
        flatMap(f andThen (Return(_)))
    }

    case class Return[F[_], A](a: A) extends Free[F, A]
    case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
    case class FlatMap[F[_], A, B](s: Free[F, A],
                                   f: A => Free[F, B]) extends Free[F, B]

    def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] =
      new Monad[({type f[a] = Free[F, a]})#f] {

        override def unit[A](a: => A): Free[F, A] = Return(a)

        override def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ma.flatMap(f)
      }

    @annotation.tailrec
    def runTrampoline[A](a: Free[Function0, A]): A = a match {
      case Return(a) => a
      case Suspend(s) => s()
      case FlatMap(x, f) => x match {
        case Return(a) => runTrampoline( f(a) )
        case Suspend(r) => runTrampoline { f(r()) }
        case FlatMap(a0,g) => runTrampoline { a0 flatMap { a0 => g(a0) flatMap f } }
      }
    }

    def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
      case Return(a) => F.unit(a)
      case Suspend(r) => r
      case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible, since `step` eliminates these cases")
    }

    @annotation.tailrec
    def step[F[_],A](a: Free[F,A]): Free[F,A] = a match {
      case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
      case FlatMap(Return(x), f) => step(f(x))
      case _ => a
    }

    sealed trait Console[A] {
      def toPar: Par[A]

      def toThunk: () => A

      // other interpreters
      def toState: ConsoleState[A]

      def toReader: ConsoleReader[A]
    }

    case object ReadLine extends Console[Option[String]] {
      def toPar = Par.lazyUnit(run)

      def toThunk = () => run

      def run: Option[String] =
        try Some(readLine())
        catch {
          case e: Exception => None
        }

      def toState = ConsoleState { bufs =>
        bufs.in match {
          case List() => (None, bufs)
          case h :: t => (Some(h), bufs.copy(in = t))
        }
      }

      def toReader = ConsoleReader { in => Some(in) }
    }

    case class PrintLine(line: String) extends Console[Unit] {
      def toPar = Par.lazyUnit(println(line))

      def toThunk = () => println(line)

      def toReader = ConsoleReader { s => () } // noop
      def toState = ConsoleState { bufs => ((), bufs.copy(out = bufs.out :+ line)) } // append to the output
    }

    object Console {
      type ConsoleIO[A] = Free[Console, A]

      def readLn: ConsoleIO[Option[String]] =
        Suspend(ReadLine)

      def printLn(line: String): ConsoleIO[Unit] =
        Suspend(PrintLine(line))
    }

    trait Translate[F[_], G[_]] {
      def apply[A](f: F[A]): G[A]
    }

    type ~>[F[_], G[_]] = Translate[F, G] // gives us infix syntax `F ~> G` for `Translate[F,G]`

    implicit val function0Monad = new Monad[Function0] {
      def unit[A](a: => A) = () => a

      def flatMap[A, B](a: Function0[A])(f: A => Function0[B]) =
        () => f(a())()
    }

    implicit val parMonad = new Monad[Par] {
      def unit[A](a: => A) = Par.unit(a)

      def flatMap[A, B](a: Par[A])(f: A => Par[B]) = Par.fork {
        Par.flatMap(a)(f)
      }
    }

    def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(
      implicit G: Monad[G]): G[A] =
      step(free) match {
        case Return(a) => G.unit(a)
        case Suspend(r) => t(r)
        case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
        case _ => sys.error("Impossible, since `step` eliminates these cases")
      }

    val consoleToFunction0 =
      new (Console ~> Function0) {
        def apply[A](a: Console[A]) = a.toThunk
      }
    val consoleToPar =
      new (Console ~> Par) {
        def apply[A](a: Console[A]) = a.toPar
      }

    def runConsoleFunction0[A](a: Free[Console, A]): () => A =
      runFree[Console, Function0, A](a)(consoleToFunction0)

    def runConsolePar[A](a: Free[Console, A]): Par[A] =
      runFree[Console, Par, A](a)(consoleToPar)

    def translate[F[_],G[_],A](f: Free[F,A])(fg: F ~> G): Free[G,A] = {
      type FreeG[A] = Free[G,A]
      val t = new (F ~> FreeG) {
        def apply[A](a: F[A]): Free[G,A] = Suspend { fg(a) }
      }
      runFree(f)(t)(freeMonad[G])
    }

    def runConsole[A](a: Free[Console,A]): A =
      runTrampoline { translate(a)(new (Console ~> Function0) {
        def apply[A](c: Console[A]) = c.toThunk
      })}


    import Console._

    case class Buffers(in: List[String], out: Vector[String])

    case class ConsoleState[A](run: Buffers => (A, Buffers)) {
      def map[B](f: A => B): ConsoleState[B] =
        ConsoleState { s =>
          val (a, s1) = run(s)
          (f(a), s1)
        }

      def flatMap[B](f: A => ConsoleState[B]): ConsoleState[B] =
        ConsoleState { s =>
          val (a, s1) = run(s)
          f(a).run(s1)
        }
    }

    object ConsoleState {
      implicit val monad = new Monad[ConsoleState] {
        def unit[A](a: => A) = ConsoleState(bufs => (a, bufs))

        def flatMap[A, B](ra: ConsoleState[A])(f: A => ConsoleState[B]) = ra flatMap f
      }
    }

    // A specialized reader monad
    case class ConsoleReader[A](run: String => A) {
      def map[B](f: A => B): ConsoleReader[B] =
        ConsoleReader(r => f(run(r)))

      def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
        ConsoleReader(r => f(run(r)).run(r))
    }

    object ConsoleReader {
      implicit val monad = new Monad[ConsoleReader] {
        def unit[A](a: => A) = ConsoleReader(_ => a)

        def flatMap[A, B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]) = ra flatMap f
      }
    }

    val consoleToState =
      new (Console ~> ConsoleState) {
        def apply[A](a: Console[A]) = a.toState
      }
    val consoleToReader =
      new (Console ~> ConsoleReader) {
        def apply[A](a: Console[A]) = a.toReader
      }

    /* Can interpet these as before to convert our `ConsoleIO` to a pure value that does no I/O! */
    def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] =
      runFree[Console, ConsoleReader, A](io)(consoleToReader)

    def runConsoleState[A](io: ConsoleIO[A]): ConsoleState[A] =
      runFree[Console, ConsoleState, A](io)(consoleToState)

    type IO[A] = Free[Par, A]

    import java.nio._
    import java.nio.channels._

    def read(file: AsynchronousFileChannel,
             fromPosition: Long,
             numBytes: Int): Par[Either[Throwable, Array[Byte]]] = ???

    def Async[A](cb: (A => Unit) => Unit): IO[A] =
      Suspend(Par.asyncF(cb))

    def IO[A](a: => A): IO[A] = Suspend {
      Par.delay(a)
    }
  }

}


