import scala.io.StdIn


//  ИНФРАСТРУКТУРА


trait Monad[M[_]]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))

// Reader
case class Reader[Env, A](run: Env => A):
  def map[B](f: A => B): Reader[Env, B] = Reader(env => f(run(env)))
  def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] = Reader(env => f(run(env)).run(env))

object Reader:
  given readerMonad[Env]: Monad[[A] =>> Reader[Env, A]] with
    def pure[A](a: A) = Reader(_ => a)
    def flatMap[A, B](ra: Reader[Env, A])(f: A => Reader[Env, B]) = Reader(env => f(ra.run(env)).run(env))

// Writer
case class Writer[A](value: A, log: Vector[String]):
  def map[B](f: A => B): Writer[B] = Writer(f(value), log)
  def flatMap[B](f: A => Writer[B]): Writer[B] =
    val res = f(value)
    Writer(res.value, log ++ res.log)

object Writer:
  def pure[A](a: A): Writer[A] = Writer(a, Vector.empty)
  def tell(msg: String): Writer[Unit] = Writer((), Vector(msg))

  given writerMonad: Monad[Writer] with
    def pure[A](a: A) = Writer(a, Vector.empty)
    def flatMap[A, B](wa: Writer[A])(f: A => Writer[B]) = wa.flatMap(f)

// State
case class State[S, A](run: S => (A, S)):
  def map[B](f: A => B): State[S, B] = flatMap(a => State.pure(f(a)))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s =>
      val (a, s2) = run(s)
      f(a).run(s2)
    )

object State:
  def pure[S, A](a: A): State[S, A] = State(s => (a, s))

  given stateMonad[S]: Monad[[A] =>> State[S, A]] with
    def pure[A](a: A) = State.pure(a)
    def flatMap[A, B](sa: State[S, A])(f: A => State[S, B]) = sa.flatMap(f)

// IO
case class IO[A](unsafeRun: () => A):
  def map[B](f: A => B): IO[B] = IO(() => f(unsafeRun()))
  def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(unsafeRun()).unsafeRun())

object IO:
  def pure[A](a: A): IO[A] = IO(() => a)
  def fromEffect[A](eff: => A): IO[A] = IO(() => eff)

  given ioMonad: Monad[IO] with
    def pure[A](a: A) = IO.pure(a)
    def flatMap[A, B](ia: IO[A])(f: A => IO[B]) = ia.flatMap(f)



// ПРЕДМЕТНАЯ ОБЛАСТЬ


case class Config(
                   moistureNorm: Map[String, Double],
                   minLight: Double,
                   growthRate: Double,
                   maturityThreshold: Double
                 )

case class Bed(id: Int, plantType: String, moisture: Double, light: Double, growth: Double)

case class GreenhouseState(
                            beds: List[Bed],
                            day: Int,
                            waterReserve: Int,
                            fertilizerReserve: Int
                          )

object GreenhouseLogic:
  def waterNeed(plantType: String, currentMoisture: Double): Reader[Config, Boolean] =
    Reader(cfg => currentMoisture < cfg.moistureNorm.getOrElse(plantType, 0.5))

  def lightNeed(plantType: String, currentLight: Double): Reader[Config, Boolean] =
    Reader(cfg => currentLight < cfg.minLight)

  def lightBed(bedId: Int): State[GreenhouseState, Unit] = State { st =>
    st.beds.find(_.id == bedId) match
      case Some(_) =>
        val newBeds = st.beds.map(b => if b.id == bedId then b.copy(light = 100.0) else b)
        ((), st.copy(beds = newBeds))
      case _ => ((), st)
  }

  def growthStep(plantType: String): Reader[Config, Double] =
    Reader(cfg => cfg.growthRate * (if plantType == "tomato" then 1.2 else 1.0))

  def canHarvest(bed: Bed): Reader[Config, Boolean] =
    Reader(cfg => bed.growth >= cfg.maturityThreshold)

  def waterBed(bedId: Int): State[GreenhouseState, Unit] = State { st =>
    st.beds.find(_.id == bedId) match
      case Some(_) if st.waterReserve > 0 =>
        val newBeds = st.beds.map(b => if b.id == bedId then b.copy(moisture = 100.0) else b)
        ((), st.copy(beds = newBeds, waterReserve = st.waterReserve - 1))
      case _ => ((), st)
  }

  def fertilizeBed(bedId: Int): State[GreenhouseState, Unit] = State { st =>
    st.beds.find(_.id == bedId) match
      case Some(_) if st.fertilizerReserve > 0 =>
        val newBeds = st.beds.map(b => if b.id == bedId then b.copy(growth = b.growth + 0.3) else b)
        ((), st.copy(beds = newBeds, fertilizerReserve = st.fertilizerReserve - 1))
      case _ => ((), st)
  }

  def nextDay: State[GreenhouseState, Unit] = State { st =>
    val newBeds = st.beds.map { bed =>
      val moisture = math.max(0.0, bed.moisture - 5.0)
      val light    = math.max(0.0, bed.light - 3.0)
      val growth   = bed.growth + 0.15
      bed.copy(moisture = moisture, light = light, growth = growth)
    }
    ((), st.copy(beds = newBeds, day = st.day + 1))
  }

  def harvest(bedId: Int): State[GreenhouseState, Option[Bed]] = State { st =>
    st.beds.find(_.id == bedId) match
      case Some(bed) =>
        (Some(bed), st.copy(beds = st.beds.filter(_.id != bedId)))
      case None => (None, st)
  }


// СЦЕНАРИЙ IO


object GreenhouseApp:
  val initialConfig: Config = Config(
    moistureNorm = Map("tomato" -> 40.0, "cucumber" -> 50.0, "pepper" -> 35.0),
    minLight = 20.0,
    growthRate = 0.5,
    maturityThreshold = 5.0
  )

  val initialState: GreenhouseState = GreenhouseState(
    beds = List(
      Bed(1, "tomato", 80.0, 60.0, 1.0),
      Bed(2, "cucumber", 70.0, 50.0, 0.5)
    ),
    day = 1,
    waterReserve = 10,
    fertilizerReserve = 5
  )

  // Вспомогательные IO-функции
  def readLineSafe: IO[String] = IO.fromEffect(StdIn.readLine())

  def readIntSafe: IO[Either[String, Int]] = IO.fromEffect {
    scala.util.Try(StdIn.readInt()).toEither.left.map(_ => "Введите корректное число")
  }

  def putStrLn(msg: String): IO[Unit] = IO.fromEffect(println(msg))
  def putStr(msg: String): IO[Unit] = IO.fromEffect(print(msg))

  // Проверка состояния грядки
  def logCheck(config: Config, bed: Bed): List[String] =
    val needWater = GreenhouseLogic.waterNeed(bed.plantType, bed.moisture).run(config)
    val needLight = GreenhouseLogic.lightNeed(bed.plantType, bed.light).run(config)
    val ready     = GreenhouseLogic.canHarvest(bed).run(config)

    List(
      if needWater then Some(s"Грядка ${bed.id} (${bed.plantType}): мало влаги (${bed.moisture}%)") else None,
      if needLight then Some(s"Грядка ${bed.id} (${bed.plantType}): мало света (${bed.light}%)") else None,
      if !ready then Some(s"Грядка ${bed.id}: урожай не готов (рост ${bed.growth} < ${config.maturityThreshold})")
      else Some(s"Грядка ${bed.id}: готова к сбору!")
    ).flatten


  //МЕНЮ И ЛОГИКА

  case class Command(key: String, label: String, execute: GreenhouseState => IO[Unit])

  def runScenario: IO[Unit] =
    // Рекурсивный цикл приложения
    def loop(st: GreenhouseState): IO[Unit] =
      // запрос ID грядки
      def promptId(currentSt: GreenhouseState)(next: Int => IO[Unit]): IO[Unit] =
        putStr("ID грядки: ").flatMap(_ => readIntSafe.flatMap {
          case Left(err) => putStrLn(err).flatMap(_ => loop(currentSt))
          case Right(id) => next(id)
        })

      // выполнение State-действия в IO-контексте
      def runStateAction[A](currentSt: GreenhouseState, action: State[GreenhouseState, A]): IO[(A, GreenhouseState)] =
        IO.fromEffect(action.run(currentSt))

      // ТАБЛИЦА КОМАНД
      val menu: List[Command] = List(
        Command("1", "полить", s => promptId(s)(id =>
          runStateAction(s, GreenhouseLogic.waterBed(id)).flatMap { case (_, ns) => loop(ns) }
        )),
        Command("2", "удобрить", s => promptId(s)(id =>
          runStateAction(s, GreenhouseLogic.fertilizeBed(id)).flatMap { case (_, ns) => loop(ns) }
        )),
        Command("3", "включить свет", s => promptId(s)(id =>
          runStateAction(s, GreenhouseLogic.lightBed(id)).flatMap { case (_, ns) => loop(ns) }
        )),
        Command("4", "следующий день", s =>
          IO.fromEffect {
            val ((), dayState) = GreenhouseLogic.nextDay.run(s)
            val msgs = dayState.beds.flatMap(b => logCheck(initialConfig, b))
            (msgs, dayState)
          }.flatMap { case (msgs, ns) =>
            msgs.foldLeft(IO.pure(()))((acc, m) => acc.flatMap(_ => putStrLn(m)))
              .flatMap(_ => loop(ns))
          }
        ),
        Command("5", "собрать урожай", s => promptId(s)(id =>
          runStateAction(s, GreenhouseLogic.harvest(id)).flatMap {
            case (Some(b), ns) if GreenhouseLogic.canHarvest(b).run(initialConfig) =>
              putStrLn(s"Собран урожай с грядки $id! Рост: ${b.growth}").flatMap(_ => loop(ns))
            case (Some(_), _) =>
              putStrLn("Растение ещё не созрело!").flatMap(_ => loop(s))
            case (None, _) =>
              putStrLn("Грядка не найдена.").flatMap(_ => loop(s))
          }
        )),
        Command("0", "выход", _ => putStrLn("До свидания!"))
      )

      // УПРАВЛЕНИЕ ЦИКЛ
      for {
        // Отображение состояния
        _ <- putStrLn(s"\n=== ДЕНЬ ${st.day} ===")
        _ <- putStrLn(s"Запасы: Вода=${st.waterReserve}, Удобрения=${st.fertilizerReserve}")
        _ <- st.beds.foldLeft(IO.pure(())) { (acc, b) =>
          acc.flatMap(_ => putStrLn(s"Грядка ${b.id}: ${b.plantType} | Влага:${b.moisture}% | Свет:${b.light}% | Рост:${b.growth}"))
        }

        //  Динамическая печать меню из данных
        _ <- putStrLn("\nВыберите действие:")
        _ <- menu.foldLeft(IO.pure(()))((acc, cmd) => acc.flatMap(_ => putStrLn(s"  [${cmd.key}] ${cmd.label}")))
        _ <- putStr("> ")

        // Ввод команды
        input <- readLineSafe

        // поиск в таблице данных -> выполнение
        _ <- menu.find(_.key == input.trim) match {
          case Some(cmd) => cmd.execute(st) // Выполняем действие из конфига
          case None      => putStrLn("Неверная команда.").flatMap(_ => loop(st))
        }
      } yield ()

    loop(initialState)

  @main def main(): Unit = runScenario.unsafeRun()