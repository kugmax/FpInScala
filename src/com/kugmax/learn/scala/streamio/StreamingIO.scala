package com.kugmax.learn.scala.streamio

import com.kugmax.learn.scala.effects.IO0.fahrenheitToCelsius
import com.kugmax.learn.scala.effects.IO1.IO
import com.kugmax.learn.scala.monad.Monad

object ImperativeAndLazyIO {

  def linesGt40k(filename: String): IO[Boolean] = IO {
    val src = io.Source.fromFile(filename)
    try {
      var count = 0
      val lines: Iterator[String] = src.getLines
      while (count <= 40000 && lines.hasNext) {
        lines.next // has side effect of advancing to next element
        count += 1
      }
      count > 40000
    }
    finally src.close
  }

  object Examples {
    val lines: Stream[String] = sys.error("defined elsewhere")
    val ex1 = lines.zipWithIndex.exists(_._2 + 1 >= 40000)
    val ex2 = lines.filter(!_.trim.isEmpty).zipWithIndex.exists(_._2 + 1 >= 40000)
    val ex3 = lines.take(40000).map(_.head).indexOfSlice("abracadabra".toList)
  }

  def lines(filename: String): IO[Stream[String]] = IO {
    val src = io.Source.fromFile(filename)
    src.getLines.toStream append { src.close; Stream.empty }
  }
}

object SimpleStreamTransducers {


  sealed trait Process[I,O] {
    import Process._

    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs) // Stream is empty
      }
      case Emit(h,t) => h #:: t(s)
    }

    def map[O2](f: O => O2): Process[I,O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) => Emit(f(h), t map f)
      case Await(recv) => Await(recv andThen (_ map f))
    }
    def ++(p: => Process[I,O]): Process[I,O] = this match {
      case Halt() => p
      case Emit(h, t) => Emit(h, t ++ p)
      case Await(recv) => Await(recv andThen (_ ++ p))
    }
    def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) => f(h) ++ t.flatMap(f)
      case Await(recv) => Await(recv andThen (_ flatMap f))
    }

    def |>[O2](p2: Process[O,O2]): Process[I,O2] = {
      p2 match {
        case Halt() => Halt()
        case Emit(h,t) => Emit(h, this |> t)
        case Await(f) => this match {
          case Emit(h,t) => t |> f(Some(h))
          case Halt() => Halt() |> f(None)
          case Await(g) => Await((i: Option[I]) => g(i) |> p2)
        }
      }
    }

    def feed(in: Seq[I]): Process[I,O] = {
      @annotation.tailrec
      def go(in: Seq[I], cur: Process[I,O]): Process[I,O] =
        cur match {
          case Halt() => Halt()
          case Await(recv) =>
            if (in.nonEmpty) go(in.tail, recv(Some(in.head)))
            else cur
          case Emit(h, t) => Emit(h, t.feed(in))
        }
      go(in, this)
    }

    def repeat: Process[I,O] = {
      def go(p: Process[I,O]): Process[I,O] = p match {
        case Halt() => go(this)
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(recv(i))
        }
        case Emit(h, t) => Emit(h, go(t))
      }
      go(this)
    }

    def repeatN(n: Int): Process[I,O] = {
      def go(n: Int, p: Process[I,O]): Process[I,O] = p match {
        case Halt() => if (n > 0) go(n-1, this) else Halt()
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(n,recv(i))
        }
        case Emit(h, t) => Emit(h, go(n,t))
      }
      go(n, this)
    }

    def filter(f: O => Boolean): Process[I,O] =
      this |> Process.filter(f)

    def zip[O2](p: Process[I,O2]): Process[I,(O,O2)] =
      Process.zip(this, p)

    def zipWithIndex: Process[I,(O,Int)] =
      this zip (count map (_ - 1))


    def orElse(p: Process[I,O]): Process[I,O] = this match {
      case Halt() => p
      case Await(recv) => Await {
        case None => p
        case x => recv(x)
      }
      case _ => this
    }
  }

  object Process {

    case class Emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]()) extends Process[I,O]
    case class Await[I,O](recv: Option[I] => Process[I,O]) extends Process[I,O]
    case class Halt[I,O]() extends Process[I,O]

    def emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
      Emit(head, tail)

    def monad[I]: Monad[({ type f[x] = Process[I,x]})#f] =
      new Monad[({ type f[x] = Process[I,x]})#f] {
        def unit[O](o: => O): Process[I,O] = emit(o)
        def flatMap[O,O2](p: Process[I,O])(f: O => Process[I,O2]): Process[I,O2] =
          p flatMap f
      }

//    implicit def toMonadic[I,O](a: Process[I,O]) = monad[I].toMonadic(a)

    def await[I,O](f: I => Process[I,O], fallback: Process[I,O] = Halt[I,O]()): Process[I,O] =
      Await[I,O] {
        case Some(i) => f(i)
        case None => fallback
      }

    def liftOne[I,O](f: I => O): Process[I,O] =
      Await {
        case Some(i) => emit(f(i))
        case None => Halt()
      }

    def lift[I,O](f: I => O): Process[I,O] =
      liftOne(f).repeat

    def filter[I](f: I => Boolean): Process[I,I] =
      Await[I,I] {
        case Some(i) if f(i) => emit(i)
        case _ => Halt()
      }.repeat

    def sum: Process[Double,Double] = {
      def go(acc: Double): Process[Double,Double] =
        await(d => emit(d+acc, go(d+acc)))
      go(0.0)
    }

    def take[I](n: Int): Process[I,I] =
      if (n <= 0) Halt()
      else await(i => emit(i, take[I](n-1)))

    def drop[I](n: Int): Process[I,I] =
      if (n <= 0) id
      else await(i => drop[I](n-1))

    def takeWhile[I](f: I => Boolean): Process[I,I] =
      await(i => if (f(i)) emit(i, takeWhile(f)) else Halt())

    def dropWhile[I](f: I => Boolean): Process[I,I] =
      await(i => if (f(i)) dropWhile(f) else emit(i, id))

    def id[I]: Process[I,I] = lift(identity)

    def count[I]: Process[I,Int] = {
      def go(acc: Int): Process[I,Int] =
        await(d => emit(acc + 1, go(acc + 1)))
      go(0)
    }

    def _count[I]: Process[I,Int] =
      lift((i: I) => 1.0) |> sum |> lift(_.toInt)

    def count2[I]: Process[I,Int] = {
      def go(n: Int): Process[I,Int] =
        await((i: I) => emit(n+1, go(n+1)))
      go(0)
    }

    def mean: Process[Double,Double] = {
      def go(sum: Double, count: Double): Process[Double,Double] =
        await((d: Double) => emit((sum+d) / (count+1), go(sum+d,count+1)))
      go(0.0, 0.0)
    }

    def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] =
      await((i: I) => f(i,z) match {
        case (o,s2) => emit(o, loop(s2)(f))
      })

    /* Exercise 4: Implement `sum` and `count` in terms of `loop` */

    def sum2: Process[Double,Double] =
      loop(0.0)( (i, s) => (i+s,i+s) )

    def count3[I]: Process[I,Int] =
      loop(0)( (i, s) => (s+1, s+1) )

    def zip[A,B,C](p1: Process[A,B], p2: Process[A,C]): Process[A,(B,C)] =
      (p1, p2) match {
        case (Halt(), _) => Halt()
        case (_, Halt()) => Halt()
        case (Emit(b, t1), Emit(c, t2)) => Emit((b,c), zip(t1, t2))
        case (Await(recv1), _) =>
          Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
        case (_, Await(recv2)) =>
          Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))
      }

    def feed[A,B](oa: Option[A])(p: Process[A,B]): Process[A,B] =
      p match {
        case Halt() => p
        case Emit(h,t) => Emit(h, feed(oa)(t))
        case Await(recv) => recv(oa)
      }

    def exists[I](f: I => Boolean): Process[I,Boolean] =
      loop(false)( (i, s) => (f(i), f(i) ) )

    def _exists[I](f: I => Boolean): Process[I,Boolean] =
      lift(f) |> any

    def any: Process[Boolean,Boolean] =
      loop(false)((b:Boolean,s) => (s || b, s || b))

    def echo[I]: Process[I,I] = await(i => emit(i))

    def skip[I,O]: Process[I,O] = await(i => Halt())
    def ignore[I,O]: Process[I,O] = skip.repeat

    def terminated[I]: Process[I,Option[I]] =
      await((i: I) => emit(Some(i), terminated[I]), emit(None))

    def processFile[A,B](f: java.io.File,
                         p: Process[String, A],
                         z: B)(g: (B, A) => B): IO[B] = IO {
      @annotation.tailrec
      def go(ss: Iterator[String], cur: Process[String, A], acc: B): B =
        cur match {
          case Halt() => acc
          case Await(recv) =>
            val next = if (ss.hasNext) recv(Some(ss.next))
            else recv(None)
            go(ss, next, acc)
          case Emit(h, t) => go(ss, t, g(acc, h))
        }
      val s = io.Source.fromFile(f)
      try go(s.getLines, p, z)
      finally s.close
    }

    def convertFahrenheit: Process[String,String] =
        lift(line => toCelsius(line.toDouble).toString)

//    def convertFahrenheit: Process[String,String] =
//      filter((line: String) => !line.startsWith("#")) |>
//        filter(line => line.trim.nonEmpty) |>
//        lift(line => toCelsius(line.toDouble).toString)


    def toCelsius(fahrenheit: Double): Double =
      (5.0 / 9.0) * (fahrenheit - 32.0)
  }
}

object GeneralizedStreamTransducers {


  trait Process[F[_],O] {
    import Process._

    def map[O2](f: O => O2): Process[F,O2] = this match {
      case Await(req,recv) =>
        Await(req, recv andThen (_ map f))
      case Emit(h, t) => Try { Emit(f(h), t map f) }
      case Halt(err) => Halt(err)
    }

    def ++(p: => Process[F,O]): Process[F,O] =
      this.onHalt {
        case End => Try(p) // we consult `p` only on normal termination
        case err => Halt(err)
      }

    def onComplete(p: => Process[F,O]): Process[F,O] =
      this.onHalt {
        case End => p.asFinalizer
        case err => p.asFinalizer ++ Halt(err) // we always run `p`, but preserve any errors
      }

    def asFinalizer: Process[F,O] = this match {
      case Emit(h, t) => Emit(h, t.asFinalizer)
      case Halt(e) => Halt(e)
      case Await(req,recv) => await(req) {
        case Left(Kill) => this.asFinalizer
        case x => recv(x)
      }
    }

    def onHalt(f: Throwable => Process[F,O]): Process[F,O] = this match {
      case Halt(e) => Try(f(e))
      case Emit(h, t) => Emit(h, t.onHalt(f))
      case Await(req,recv) => Await(req, recv andThen (_.onHalt(f)))
    }

    def flatMap[O2](f: O => Process[F,O2]): Process[F,O2] =
      this match {
        case Halt(err) => Halt(err)
        case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
        case Await(req,recv) =>
          Await(req, recv andThen (_ flatMap f))
      }

    def repeat: Process[F,O] =
      this ++ this.repeat

    def repeatNonempty: Process[F,O] = {
      val cycle = (this.map(o => Some(o): Option[O]) ++ emit(None)).repeat
      val trimmed = cycle |> window2 |> (takeWhile {
        case (Some(None), None) => false
        case _ => true
      })
      trimmed.map(_._2).flatMap {
        case None => Halt(End)
        case Some(o) => emit(o)
      }
    }

    trait MonadCatch[F[_]] extends Monad[F] {
      def attempt[A](a: F[A]): F[Either[Throwable,A]]
      def fail[A](t: Throwable): F[A]
    }

    def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
      def go(cur: Process[F,O], acc: IndexedSeq[O]): F[IndexedSeq[O]] =
        cur match {
          case Emit(h,t) => go(t, acc :+ h)
          case Halt(End) => F.unit(acc)
          case Halt(err) => F.fail(err)
          case Await(req,recv) => F.flatMap (F.attempt(req)) { e => go(Try(recv(e)), acc) }
        }
      go(this, IndexedSeq())
    }

    def |>[O2](p2: Process1[O,O2]): Process[F,O2] = {
      p2 match {
        case Halt(e) => this.kill onHalt { e2 => Halt(e) ++ Halt(e2) }
        case Emit(h, t) => Emit(h, this |> t)
        case Await(req,recv) => this match {
          case Halt(err) => Halt(err) |> recv(Left(err))
          case Emit(h,t) => t |> Try(recv(Right(h)))
          case Await(req0,recv0) => await(req0)(recv0 andThen (_ |> p2))
        }
      }
    }

    @annotation.tailrec
    final def kill[O2]: Process[F,O2] = this match {
      case Await(req,recv) => recv(Left(Kill)).drain.onHalt {
        case Kill => Halt(End) // we convert the `Kill` exception back to normal termination
        case e => Halt(e)
      }
      case Halt(e) => Halt(e)
      case Emit(h, t) => t.kill
    }

    /** Alias for `this |> p2`. */
    def pipe[O2](p2: Process1[O,O2]): Process[F,O2] =
      this |> p2

    final def drain[O2]: Process[F,O2] = this match {
      case Halt(e) => Halt(e)
      case Emit(h, t) => t.drain
      case Await(req,recv) => Await(req, recv andThen (_.drain))
    }

    def filter(f: O => Boolean): Process[F,O] =
      this |> Process.filter(f)

    def take(n: Int): Process[F,O] =
      this |> Process.take(n)

    def once: Process[F,O] = take(1)

    def tee[O2,O3](p2: Process[F,O2])(t: Tee[O,O2,O3]): Process[F,O3] = {
      t match {
        case Halt(e) => this.kill onComplete p2.kill onComplete Halt(e)
        case Emit(h,t) => Emit(h, (this tee p2)(t))
        case Await(side, recv) => side.get match {
          case Left(isO) => this match {
            case Halt(e) => p2.kill onComplete Halt(e)
            case Emit(o,ot) => (ot tee p2)(Try(recv(Right(o))))
            case Await(reqL, recvL) =>
              await(reqL)(recvL andThen (this2 => this2.tee(p2)(t)))
          }
          case Right(isO2) => p2 match {
            case Halt(e) => this.kill onComplete Halt(e)
            case Emit(o2,ot) => (this tee ot)(Try(recv(Right(o2))))
            case Await(reqR, recvR) =>
              await(reqR)(recvR andThen (p3 => this.tee(p3)(t)))
          }
        }
      }
    }

    def zipWith[O2,O3](p2: Process[F,O2])(f: (O,O2) => O3): Process[F,O3] =
      (this tee p2)(Process.zipWith(f))

    def zip[O2](p2: Process[F,O2]): Process[F,(O,O2)] =
      zipWith(p2)((_,_))

    def to[O2](sink: Sink[F,O]): Process[F,Unit] =
      join { (this zipWith sink)((o,f) => f(o)) }

    def through[O2](p2: Channel[F, O, O2]): Process[F,O2] =
      join { (this zipWith p2)((o,f) => f(o)) }
  }

  object Process {
    case class Await[F[_],A,O](
                                req: F[A],
                                recv: Either[Throwable,A] => Process[F,O]) extends Process[F,O]

    case class Emit[F[_],O](
                             head: O,
                             tail: Process[F,O]) extends Process[F,O]

    case class Halt[F[_],O](err: Throwable) extends Process[F,O]

    def emit[F[_],O](
                      head: O,
                      tail: Process[F,O] = Halt[F,O](End)): Process[F,O] =
      Emit(head, tail)

    def await[F[_],A,O](req: F[A])(recv: Either[Throwable,A] => Process[F,O]): Process[F,O] =
      Await(req, recv)

    def Try[F[_],O](p: => Process[F,O]): Process[F,O] =
      try p
      catch { case e: Throwable => Halt(e) }

    def TryOr[F[_],O](p: => Process[F,O])(cleanup: Process[F,O]): Process[F,O] =
      try p
      catch { case e: Throwable => cleanup ++ Halt(e) }
    def TryAwait[F[_],O](p: => Process[F,O])(fallback: Process[F,O], cleanup: Process[F,O]): Process[F,O] =
      try p
      catch {
        case End => fallback
        case e: Throwable => cleanup ++ Halt(e)
      }

    case object End extends Exception

    case object Kill extends Exception

    def runLog[O](src: Process[IO,O]): IO[IndexedSeq[O]] = IO {
      val E = java.util.concurrent.Executors.newFixedThreadPool(4)
      @annotation.tailrec
      def go(cur: Process[IO,O], acc: IndexedSeq[O]): IndexedSeq[O] =
        cur match {
          case Emit(h,t) => go(t, acc :+ h)
          case Halt(End) => acc
          case Halt(err) => throw err
          case Await(req,recv) =>
            val next =
              try recv(Right(unsafePerformIO(req)(E)))
              catch { case err: Throwable => recv(Left(err)) }
            go(next, acc)
        }
      try go(src, IndexedSeq())
      finally E.shutdown
    }

    import java.io.{BufferedReader,FileReader}
    val p: Process[IO, String] =
      await(IO(new BufferedReader(new FileReader("lines.txt")))) {
        case Right(b) =>
          lazy val next: Process[IO,String] = await(IO(b.readLine)) {
            case Left(e) => await(IO(b.close))(_ => Halt(e))
            case Right(line) => Emit(line, next)
          }
          next
        case Left(e) => Halt(e)
      }

    def resource[R,O](acquire: IO[R])(
      use: R => Process[IO,O])(
                       release: R => Process[IO,O]): Process[IO,O] =
      eval(acquire) flatMap { r => use(r).onComplete(release(r)) }

    def resource_[R,O](acquire: IO[R])(
      use: R => Process[IO,O])(
                        release: R => IO[Unit]): Process[IO,O] =
      resource(acquire)(use)(release andThen (eval_[IO,Unit,O]))

    def lines(filename: String): Process[IO,String] =
      resource
      { IO(io.Source.fromFile(filename)) }
      { src =>
        lazy val iter = src.getLines // a stateful iterator
        def step = if (iter.hasNext) Some(iter.next) else None
        lazy val lines: Process[IO,String] = eval(IO(step)).flatMap {
          case None => Halt(End)
          case Some(line) => Emit(line, lines)
        }
        lines
      }
      { src => eval_ { IO(src.close) } }

    def eval[F[_],A](a: F[A]): Process[F,A] =
      await[F,A,A](a) {
        case Left(err) => Halt(err)
        case Right(a) => Emit(a, Halt(End))
      }

    def eval_[F[_],A,B](a: F[A]): Process[F,B] =
      eval[F,A](a).drain[B]

    def evalIO[A](a: IO[A]): Process[IO,A] =
      eval[IO,A](a)

    case class Is[I]() {
      sealed trait f[X]
      val Get = new f[I] {}
    }
    def Get[I] = Is[I]().Get

    type Process1[I,O] = Process[Is[I]#f, O]


    def await1[I,O](
                     recv: I => Process1[I,O],
                     fallback: => Process1[I,O] = halt1[I,O]): Process1[I, O] =
      Await(Get[I], (e: Either[Throwable,I]) => e match {
        case Left(End) => fallback
        case Left(err) => Halt(err)
        case Right(i) => Try(recv(i))
      })

    def emit1[I,O](h: O, tl: Process1[I,O] = halt1[I,O]): Process1[I,O] =
      emit(h, tl)

    def halt1[I,O]: Process1[I,O] = Halt[Is[I]#f, O](End)

    def lift[I,O](f: I => O): Process1[I,O] =
      await1[I,O]((i:I) => emit(f(i))) repeat

    def filter[I](f: I => Boolean): Process1[I,I] =
      await1[I,I](i => if (f(i)) emit(i) else halt1) repeat

    def take[I](n: Int): Process1[I,I] =
      if (n <= 0) halt1
      else await1[I,I](i => emit(i, take(n-1)))

    def takeWhile[I](f: I => Boolean): Process1[I,I] =
      await1(i =>
        if (f(i)) emit(i, takeWhile(f))
        else      halt1)

    def dropWhile[I](f: I => Boolean): Process1[I,I] =
      await1(i =>
        if (f(i)) dropWhile(f)
        else      emit(i,id))

    def id[I]: Process1[I,I] =
      await1((i: I) => emit(i, id))

    def window2[I]: Process1[I,(Option[I],I)] = {
      def go(prev: Option[I]): Process1[I,(Option[I],I)] =
        await1[I,(Option[I],I)](i => emit(prev -> i) ++ go(Some(i)))
      go(None)
    }

    /** Emits `sep` in between each input received. */
    def intersperse[I](sep: I): Process1[I,I] =
      await1[I,I](i => emit1(i) ++ id.flatMap(i => emit1(sep) ++ emit1(i)))

    case class T[I,I2]() {
      sealed trait f[X] { def get: Either[I => X, I2 => X] }
      val L = new f[I] { def get = Left(identity) }
      val R = new f[I2] { def get = Right(identity) }
    }
    def L[I,I2] = T[I,I2]().L
    def R[I,I2] = T[I,I2]().R

    type Tee[I,I2,O] = Process[T[I,I2]#f, O]


    def haltT[I,I2,O]: Tee[I,I2,O] =
      Halt[T[I,I2]#f,O](End)

    def awaitL[I,I2,O](recv: I => Tee[I,I2,O],
                       fallback: => Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] =
      await[T[I,I2]#f,I,O](L) {
        case Left(End) => fallback
        case Left(err) => Halt(err)
        case Right(a) => Try(recv(a))
      }

    def awaitR[I,I2,O](recv: I2 => Tee[I,I2,O],
                       fallback: => Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] =
      await[T[I,I2]#f,I2,O](R) {
        case Left(End) => fallback
        case Left(err) => Halt(err)
        case Right(a) => Try(recv(a))
      }

    def emitT[I,I2,O](h: O, tl: Tee[I,I2,O] = haltT[I,I2,O]): Tee[I,I2,O] =
      emit(h, tl)

    def zipWith[I,I2,O](f: (I,I2) => O): Tee[I,I2,O] =
      awaitL[I,I2,O](i  =>
        awaitR        (i2 => emitT(f(i,i2)))) repeat

    def zip[I,I2]: Tee[I,I2,(I,I2)] = zipWith((_,_))

    def passR[I,I2]: Tee[I,I2,I2] = awaitR(emitT(_, passR))

    def passL[I,I2]: Tee[I,I2,I] = awaitL(emitT(_, passL))

    def interleaveT[I]: Tee[I,I,I] =
      awaitL[I,I,I](i =>
        awaitR       (i2 => emitT(i) ++ emitT(i2))) repeat

    type Sink[F[_],O] = Process[F, O => Process[F,Unit]]

    import java.io.FileWriter

    /* A `Sink` which writes input strings to the given file. */
    def fileW(file: String, append: Boolean = false): Sink[IO,String] =
      resource[FileWriter, String => Process[IO,Unit]]
        { IO { new FileWriter(file, append) }}
        { w => constant { (s: String) => eval[IO,Unit](IO(w.write(s))) }}
        { w => eval_(IO(w.close)) }

    /* The infinite, constant stream. */
    def constant[A](a: A): Process[IO,A] =
      eval(IO(a)).flatMap { a => Emit(a, constant(a)) }

    def join[F[_],A](p: Process[F,Process[F,A]]): Process[F,A] =
      p.flatMap(a => a)

    val converter: Process[IO,Unit] =
      lines("fahrenheit.txt").
        filter(line => !line.startsWith("#") && !line.trim.isEmpty).
        map(line => fahrenheitToCelsius(line.toDouble).toString).
        pipe(intersperse("\n")).
        to(fileW("celsius.txt")).
        drain


    type Channel[F[_],I,O] = Process[F, I => Process[F,O]]
    import java.sql.{Connection, PreparedStatement, ResultSet}

    def query(conn: IO[Connection]):
    Channel[IO, Connection => PreparedStatement, Map[String,Any]] =
      resource_
      { conn }
      { conn => constant { (q: Connection => PreparedStatement) =>
        resource_
        { IO {
          val rs = q(conn).executeQuery
          val ncols = rs.getMetaData.getColumnCount
          val cols = (1 to ncols).map(rs.getMetaData.getColumnName)
          (rs, cols)
        }}
        { case (rs, cols) =>
          def step =
            if (!rs.next) None
            else Some(cols.map(c => (c, rs.getObject(c): Any)).toMap)
          lazy val rows: Process[IO,Map[String,Any]] =
            eval(IO(step)).flatMap {
              case None => Halt(End)
              case Some(row) => Emit(row, rows)
            }
          rows
        }
        { p => IO { p._1.close } } // close the ResultSet
      }}
      { c => IO(c.close) }


    val convertAll: Process[IO,Unit] = (for {
      out <- fileW("celsius.txt").once
      file <- lines("fahrenheits.txt")
      _ <- lines(file).
        map(line => fahrenheitToCelsius(line.toDouble)).
        flatMap(celsius => out(celsius.toString))
    } yield ()) drain

    val convertMultisink: Process[IO,Unit] = (for {
      file <- lines("fahrenheits.txt")
      _ <- lines(file).
        map(line => fahrenheitToCelsius(line.toDouble)).
        map(_ toString).
        to(fileW(file + ".celsius"))
    } yield ()) drain

    val convertMultisink2: Process[IO,Unit] = (for {
      file <- lines("fahrenheits.txt")
      _ <- lines(file).
        filter(!_.startsWith("#")).
        map(line => fahrenheitToCelsius(line.toDouble)).
        filter(_ > 0).
        map(_ toString).
        to(fileW(file + ".celsius"))
    } yield ()) drain
  }
}

object ProcessTest extends App {
  import GeneralizedStreamTransducers._
  import Process._

  val p = eval(IO { println("woot"); 1 }).repeat
  val p2 = eval(IO { println("cleanup"); 2 } ).onHalt {
    case Kill => println { "cleanup was killed, instead of bring run" }; Halt(Kill)
    case e => Halt(e)
  }

  println { Process.runLog { p2.onComplete(p2).onComplete(p2).take(1).take(1) } }
  println { Process.runLog(converter) }
  // println { Process.collect(Process.convertAll) }
}