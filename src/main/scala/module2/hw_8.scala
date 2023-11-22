package module2

import cats.Functor

import java.time.LocalDate
import java.time.format.DateTimeFormatter

object hw_8 {
  trait Show[T] {
    def show(value: T): String
  }

  object ShowSyntax {
    def apply[T](implicit s: Show[T]): Show[T] = s

    def from[T]: Show[T] = new Show[T] {
      override def show(value: T): String = "Show[T]: " + value
    }

    // инстансы
    implicit val stringShow: Show[String] = from[String]
    implicit val intShow: Show[Int] = from[Int]
    implicit val dateShow: Show[LocalDate] = from[LocalDate]
    implicit val booleanShow: Show[Boolean] = from[Boolean]

    implicit class ShowOps[T](value: T) {
      def show(implicit s: Show[T]): String = apply(s).show(value)
    }
  }

  def main(args: Array[String]): Unit = {
    import ShowSyntax._

    val a: String = "test"
    println(a.show)
    println(a.show("test: " + _))

    val b = 1
    println(b.show)
    println(b.show( (x: Int) => "Show function: " + x) )

    val dateStr = "2023-01-01"
    val c = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val date: LocalDate = LocalDate.parse(dateStr, c)
    println(date.show)
    println(date.show ((x: LocalDate) => "Show function: " + x) )

    val d: Boolean = true
    println(d.show)
    println(d.show((x: Boolean) => "Show function: " + x ) )

  }


  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = flatten(map(fa)(a => f(a)))

    def point[A](a: A): F[A]

    def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(a => a)
  }

  object Monad {
    implicit def monadOption[T]: Monad[Option] = new Monad[Option] {
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

      override def point[A](a: A): Option[A] = Option(a)

      override def flatten[A](fa: Option[Option[A]]): Option[A] = fa.flatten
    }
  }
}
