package module1.hw1

import scala.annotation.tailrec

/** Реализовать односвязанный иммутабельный список List
 * Список имеет два случая:
 * Nil - пустой список
 * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список) */
sealed trait List[+T] {
  /** Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::` */
  def ::[TT >: T](elem: TT): List[TT] = Cons(elem, this)

  def isEmpty: Boolean = {
    this match {
      case Cons(_, _) => false
      case Nil => true
    }
  }

  /** Метод mkString возвращает строковое представление списка, с учетом переданного разделителя */
  def mkString(separator: String): String = {
    val result: String = ""

    @tailrec
    def loop(input: List[T], result: String): String = {
      input match {
        case Nil => result
        case Cons(head, Nil) => result + head
        case Cons(head, tail) => loop(tail, result + head + separator)
      }
    }

    loop(this, result)
  }

  /** метод reverse который позволит заменить порядок элементов в списке на противоположный
   */
  def reverse(): List[T] = {
    val result = Nil
    val current_node = this

    @tailrec
    def loop(current_node: List[T], result: List[T]): List[T] = {
      current_node match {
        case Cons(head, tail) => loop(tail, head :: result)
        case Nil => result
      }
    }

    loop(current_node, result)
  }

  /** Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка */
  def map[B](f: T => B): List[B] = {
    val result: List[B] = Nil

    @tailrec
    def loop(input: List[T], result: List[B]): List[B] = {
      input match {
        case Nil => result
        case Cons(head, tail) => loop(tail, f(head) :: result)
      }
    }

    loop(this.reverse(), result)
  }

  /** Реализовать метод filter для списка который будет фильтровать список по некому условию */
  def filter(f: T => Boolean) : List[T] = {
    val result = Nil

    @tailrec
    def loop(input: List[T], result: List[T]): List[T] = {
      input match {
        case Nil => result
        case Cons(head, tail) if f(head) => loop(tail, head :: result)
        case Cons(head, tail) if !f(head) => loop(tail, result)
      }
    }

    loop(this, result)
  }
}

case class Cons[A](head: A, tail: List[A]) extends List[A]
case object Nil extends List[Nothing]

object List {
  /**
   * Конструктор, позволяющий создать список из N - го числа аргументов
   * Для этого можно воспользоваться *
   *
   * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
   * def printArgs(args: Int*) = args.foreach(println(_)) */
  def apply[A](v: A*): List[A] = {
    if(v.isEmpty) Nil
    else Cons(v.head, apply(v.tail: _*))
  }

  /** Написать функцию incList котрая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */
  def incList(input: List[Int]): List[Int] = {
    input.map(_ + 1)
  }

  /** Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */
  def shoutString(input: List[String]): List[String] = {
    input.map(_ + "!")
  }
}




















