package catsconcurrency.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import Wallet._

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file._

import scala.io._
import scala.language.higherKinds




// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {
  override def balance: F[BigDecimal] = for {
    balance <- readFile(id)
  } yield balance
  def topup(amount: BigDecimal): F[Unit] = for {
    balanceValue <- this.balance
    _ <- putDataToFile(id, balanceValue + amount)
  } yield ()

  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = for {
    balanceValue <- this.balance
    result <- if(balanceValue - amount > 0) putDataToFile(id, balanceValue - amount).map(Right(_)) else Sync[F].delay(Left(BalanceTooLow))
  } yield result
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] = for {
    // создание нового кошелька
    _ <- createFile(id)
    // создание объекта кошелька
    res <- Sync[F].delay(new FileWallet(id))
  } yield res

  private def createFile[F[_]: Sync](id: WalletId): F[Unit] = {
    if(!Files.exists(Paths.get(id))) {
      for {
        // создание нового файла
        _ <- Sync[F].delay(new File(id).createNewFile())
        // инициализируем сумму в кошельке значением 0
        _ <- putDataToFile(id, 0)
      } yield ()
    } else {
      Sync[F].unit
    }
  }

  def putDataToFile[F[_]: Sync](id: WalletId, value: BigDecimal): F[Unit] = Sync[F].bracket
    { openFile(id) }
    { _: BufferedSource =>
      Sync[F].delay(Files.write(Paths.get(id), value.toString().getBytes(StandardCharsets.UTF_8))) *>
        Sync[F].unit }
    { x => closeFile(x) }

  def readFile[F[_]: Sync](id: WalletId): F[BigDecimal] = Sync[F].bracket
    { openFile(id) }
    { v => Sync[F].delay(BigDecimal(v.getLines().mkString)) }
    { x => closeFile(x) }

  private def openFile[F[_]: Sync](id: WalletId): F[BufferedSource] = Sync[F].delay(Source.fromFile(id))

  private def closeFile[F[_]: Sync](f: BufferedSource): F[Unit] = Sync[F].delay(f.close())

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}
