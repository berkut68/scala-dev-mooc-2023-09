package module3


import zio.{IO, Task, UIO, ULayer, URIO, ZIO, clock, random}
import zio.clock.Clock
import zio.console._
import zio.random.{Random, _}

import zio.duration._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */
  lazy val guessProgram = {
    def readNumberFromConsole(console: Console.Service): Task[Int] = {1
      lazy val readLine: IO[IOException, String] = console.getStrLn
      lazy val readInt: Task[Int] = readLine.flatMap(str => ZIO.effect(str.toInt))
      lazy val readIntOrRetry: Task[Int] = readInt.orElse {
        ZIO.effect(println("Некорректный ввод, попробуйте снова")).zipRight(readIntOrRetry)
      }

      console.putStrLn("Введите число от 1 до 3 включительно: ") *> readIntOrRetry
    }

    def checkUsersNumber(usersNumber: Int, console: Console.Service): ZIO[Any, Throwable, Int] = {
      lazy val checkUsersNumber = if (usersNumber > 3 || usersNumber < 1) {
        console.putStrLn("Число должно быть от 1 до 3 включительно") *>
          ZIO.fail(new Throwable("Error"))
      } else ZIO.succeed(usersNumber)

      checkUsersNumber
    }

    for {
      console: Console.Service <- ZIO.environment[Console].map(_.get)
      random <- ZIO.environment[Random].map(_.get)
      numberToGuess <- random.nextIntBetween(1, 4)
      _ <- console.putStrLn("Задуманное число: " + numberToGuess.toString)
      usersNumber <- readNumberFromConsole(console)
      usersNumberProcessed <- checkUsersNumber(usersNumber, console)
      _ <- console.putStr(if (usersNumberProcessed == numberToGuess) "Верно! Вы угадали загаданное число" else s"Неверно! Загаданное число: ${numberToGuess}\n")
    } yield ()
  }

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile: Task[Boolean] => Task[Unit] =
    (effect: Task[Boolean]) => for {
      value <- effect
      _ <- if (value) ZIO.succeed() else doWhile(effect)
    } yield ()

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault: ZIO[Any, Throwable, config.AppConfig] = for {
    c <- config.load.foldM(
      _ => ZIO.succeed(config.AppConfig("127.0.0.1", "8080")),
      success => ZIO.succeed(success)
    )
    _ <- ZIO.effect(println(c.host, c.port))
  } yield c


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: ZIO[Random with Clock, Nothing, Int] = ZIO.sleep(1 seconds) zipRight random.nextIntBetween(0, 10)


  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: List[ZIO[Random with Clock, Nothing, Int]] = List.fill(10)(eff)


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val zero: Task[Int] = ZIO.effect(0)

  lazy val sumEffects: ZIO[Random with Clock, Throwable, Task[Int]] =
    ZIO.foldLeft(effects)(zero)((z, i) => z.flatMap(z1 => i.map(i1 => ZIO.effect(z1 + i1))))

  lazy val sumAll: ZIO[Random with Clock, Throwable, Int] = sumEffects.flatMap(a => a.map(b => b))

  lazy val result = sumAll.flatMap(e => putStrLn(s"Sum of all elements in list: ${e.toString}"))

  lazy val currTime: URIO[Clock, Long] = clock.currentTime(TimeUnit.SECONDS)

  def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A] = for {
    start <- currTime
    r <- zio
    end <- currTime
    _ <- putStrLn(s"Running time: ${end - start}")
  } yield r

  lazy val app = printEffectRunningTime(result)


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val collPar: ZIO[Random with Clock, Nothing, List[Int]] = ZIO.collectAllPar(effects)

  lazy val sum2: ZIO[Random with Clock, Throwable, Int] = collPar.flatMap(a => ZIO.effect(a.sum))

  lazy val result2 = sum2.flatMap(a => putStrLn(s"Sum of all elements in list: ${a.toString}"))

  lazy val appSpeedUp = printEffectRunningTime(result2)


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.console.putStrLn например
   */


  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
   *
   *
   */

  lazy val appWithTimeLogg = ???

  /**
   *
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */

  lazy val runApp = ???

}
