// Main.scala
// IO-сценарий ввод/вывод, меню, главный цикл

import scala.io.StdIn

object GreenhouseApp:

  //Начальные данные

  val config: Config = Config(
    moistureNorm = Map("tomato" -> 40, "cucumber" -> 50),
    minLight = 20, growthRate = 0.5, maturityThreshold = 5
  )

  val startState: GreenhouseState = GreenhouseState(
    beds = List(
      Bed(1, "tomato", 80, 60, 1),
      Bed(2, "cucumber", 70, 50, 0.5)
    ),
    day = 1, waterReserve = 10, fertilizerReserve = 5
  )

  //IO-действия 

  def readLine: IO[String] = IO.fromEffect(StdIn.readLine())

  def readInt: IO[Option[Int]] = IO.fromEffect(
    scala.util.Try(StdIn.readInt()).toOption
  )

  def print(msg: String): IO[Unit] = IO.fromEffect(println(msg))

  def printNoNL(msg: String): IO[Unit] = IO.fromEffect(scala.Console.print(msg))

  //Проверка грядки возвращает список сообщений

  def checkBed(bed: Bed): List[String] =
    val needWater = GreenhouseLogic.waterNeed(bed.plantType, bed.moisture).run(config)
    val needLight = GreenhouseLogic.lightNeed(bed.plantType, bed.light).run(config)
    val ready = GreenhouseLogic.canHarvest(bed).run(config)

    List(
      if needWater then Some(s" Грядка ${bed.id}: мало влаги") else None,
      if needLight then Some(s" Грядка ${bed.id}: мало света") else None,
      if !ready then Some(s" Грядка ${bed.id}: ещё растёт (${bed.growth}/5)")
      else Some(s" Грядка ${bed.id}: готова к сбору!")
    ).flatten

  // печать списка

  def printList[T](items: List[T], doItem: T => IO[Unit]): IO[Unit] =
    if items.isEmpty then IO.pure(())
    else doItem(items.head).flatMap(_ => printList(items.tail, doItem))

  // Главный запуск 

  def run: IO[Unit] =

    // Рекурсивный цикл приложения
    def loop(st: GreenhouseState): IO[Unit] =

      // Запросить ID грядки и выполнить действие
      def askId(doAction: Int => IO[Unit]): IO[Unit] =
        printNoNL("ID грядки: ").flatMap(_ =>
          readInt.flatMap { idOpt =>
            if idOpt.isDefined then doAction(idOpt.get)
            else print(" Введите число!").flatMap(_ => loop(st))
          }
        )

      // Меню команд 

      val menu: List[Cmd] = List(

        Cmd("1", "полить", s => askId { id =>
          IO.fromEffect {
            val ((), newState) = GreenhouseLogic.waterBed(id).run(s)
            newState
          }.flatMap(loop)
        }),

        Cmd("2", "удобрить", s => askId { id =>
          IO.fromEffect {
            val ((), newState) = GreenhouseLogic.fertilizeBed(id).run(s)
            newState
          }.flatMap(loop)
        }),

        Cmd("3", "включить свет", s => askId { id =>
          IO.fromEffect {
            val ((), newState) = GreenhouseLogic.lightBed(id).run(s)
            newState
          }.flatMap(loop)
        }),

        Cmd("4", "следующий день", s =>
          IO.fromEffect(GreenhouseLogic.nextDay.run(s)).flatMap { result =>
            val newSt = result._2
            val logs = newSt.beds.flatMap(checkBed)
            printList(logs, print).flatMap(_ => loop(newSt))
          }
        ),

        Cmd("5", "собрать урожай", s => askId { id =>
          IO.fromEffect(GreenhouseLogic.harvest(id).run(s)).flatMap { result =>
            val bedOpt = result._1
            val newSt = result._2
            if bedOpt.isDefined && GreenhouseLogic.canHarvest(bedOpt.get).run(config) then
              print(s" Урожай с грядки $id! Рост: ${bedOpt.get.growth}")
                .flatMap(_ => loop(newSt))
            else if bedOpt.isDefined then
              print(" Ещё не созрело!").flatMap(_ => loop(s))
            else
              print(" Грядка не найдена").flatMap(_ => loop(s))
          }
        }),

        Cmd("0", "выход", _ => print(" Пока!"))
      )

      // Один шаг цикла показать → получить ввод → выполнить 

      for
        // Показать состояние
        _ <- print(s"\n=== ДЕНЬ ${st.day} ===")
        _ <- print(s"Запасы: ${st.waterReserve} вода ${st.fertilizerReserve} удобрения")
        _ <- printList(
          st.beds.map(b => s" ${b.id}: ${b.plantType} | ${b.moisture}% вода ${b.light}% свет ${b.growth} рост"),
          print
        )

        // Показать меню
        _ <- print("\nДействия:")
        _ <- printList(menu.map(c => s"  [${c.key}] ${c.text}"), print)
        _ <- printNoNL("> ")

        // Получить и обработать ввод
        input <- readLine
        _ <- menu.find(_.key == input.trim)
          .fold(
            print(" Неверная команда").flatMap(_ => loop(st))
          )(
            cmd => cmd.action(st)
          )
      yield ()

    // Запустить цикл с начальным состоянием
    loop(startState)

  // Точка входа
  @main def main(): Unit = run.unsafeRun()