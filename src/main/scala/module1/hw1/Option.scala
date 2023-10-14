package module1.hw1

sealed trait Option[+T] {
  def isEmpty: Boolean = this match {
    case None => true
    case Some(v) => false
  }

  def get: T = this match {
    case Some(v) => v
    case None => throw new Exception("get on empty Option")
  }

  def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

  def flatMap[B](f: T => Option[B]): Option[B] = this match {
    case Some(v) => f(v)
    case None => None
  }

  /** Реализовать метод printIfAny, который будет печатать значение, если оно есть*/
  def printIfAny(): Unit = {
    this match {
      case Some(v) => print(v)
      case None => None
    }
  }

  /**
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   * */
  def filter[B](f: T => Boolean): Option[T] = {
      this match {
        case Some(v) if f(v) && !isEmpty => Some(v)
        case _ => None
      }
  }
}
case class Some[T](v: T) extends Option[T]
case object None extends Option[Nothing]

object Option {
  def apply[T](v: T): Option[T] = Some(v)

  // Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
  def zip[A, B](a: Option[A], b: Option[B]): Option[(A, B)] = Option(a.get, b.get)
}