// Monads.scala
// Базовые монады 
trait Monad[M[_]]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))

// Reader вычисления, зависящие от конфигурации
case class Reader[Env, A](run: Env => A):
  def map[B](f: A => B): Reader[Env, B] = Reader(env => f(run(env)))
  def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] =
    Reader(env => f(run(env)).run(env))

object Reader:
  given [Env]: Monad[[A] =>> Reader[Env, A]] with
    def pure[A](a: A) = Reader(_ => a)
    def flatMap[A, B](ra: Reader[Env, A])(f: A => Reader[Env, B]) =
      Reader(env => f(ra.run(env)).run(env))

// Writer вычисления с накоплением лога
case class Writer[A](value: A, log: Vector[String]):
  def map[B](f: A => B): Writer[B] = Writer(f(value), log)
  def flatMap[B](f: A => Writer[B]): Writer[B] =
    val res = f(value)
    Writer(res.value, log ++ res.log)

object Writer:
  def pure[A](a: A): Writer[A] = Writer(a, Vector.empty)
  def tell(msg: String): Writer[Unit] = Writer((), Vector(msg))
  given Monad[Writer] with
    def pure[A](a: A) = Writer(a, Vector.empty)
    def flatMap[A, B](wa: Writer[A])(f: A => Writer[B]) = wa.flatMap(f)

// State вычисления с изменяемым состоянием
case class State[S, A](run: S => (A, S)):
  def map[B](f: A => B): State[S, B] = flatMap(a => State.pure(f(a)))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    )

object State:
  def pure[S, A](a: A): State[S, A] = State(s => (a, s))
  given [S]: Monad[[A] =>> State[S, A]] with
    def pure[A](a: A) = State.pure(a)
    def flatMap[A, B](sa: State[S, A])(f: A => State[S, B]) = sa.flatMap(f)

// IO отложенные побочные эффекты
case class IO[A](unsafeRun: () => A):
  def map[B](f: A => B): IO[B] = IO(() => f(unsafeRun()))
  def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(unsafeRun()).unsafeRun())

object IO:
  def pure[A](a: A): IO[A] = IO(() => a)
  def fromEffect[A](eff: => A): IO[A] = IO(() => eff)
  given Monad[IO] with
    def pure[A](a: A) = IO.pure(a)
    def flatMap[A, B](ia: IO[A])(f: A => IO[B]) = ia.flatMap(f)