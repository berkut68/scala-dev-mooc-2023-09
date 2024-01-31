package catsconcurrency.cats_effect_homework

import cats.Monad
import cats.effect._
import cats.implicits._
import Wallet._

import scala.concurrent.duration._
import scala.language.postfixOps

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1.txt")
      wallet2 <- Wallet.fileWallet[IO]("2.txt")
      wallet3 <- Wallet.fileWallet[IO]("3.txt")
      f1 <- Spawn[IO].start(loop(topup(wallet1, 100)))
      f2 <- Spawn[IO].start(loop(topup(wallet2, 500)))
      f3 <- Spawn[IO].start(loop(topup(wallet3, 2000)))
      printer <- Spawn[IO].start(loop(printBalances(wallet1, wallet2, wallet3)))
      _ <- IO.readLine
      _ <- f1.cancel *> f2.cancel *> f3.cancel *> printer.cancel
    } yield ()

  private def topup(wallet: Wallet[IO], pause: Int): IO[Unit] = for {
    _ <- wallet.topup(100)
    res <- IO.sleep(pause milliseconds)
  } yield res

  private def printBalances(w1: Wallet[IO], w2: Wallet[IO], w3: Wallet[IO]): IO[Unit] = for {
    _ <- w1.balance.flatMap(sum => IO.println(s"wallet1: $sum"))
    _ <- w2.balance.flatMap(sum => IO.println(s"wallet2: $sum"))
    _ <- w3.balance.flatMap(sum => IO.println(s"wallet3: $sum"))
    _ <- IO.sleep(1 second)
  } yield ()

  private def loop(f: => IO[Unit]): IO[Unit] =
    Monad[IO].whileM_(IO(true)) {
      f
    }
}
