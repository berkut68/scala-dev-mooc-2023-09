package module2

object higher_kinded_types{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap{ a => b.map((a, _))}


  def tuplef[F[_], A, B](fa: F[A], fb: F[B])(implicit bindable: Bindable[F]): F[(A, B)] =
    bindable.flatMap(fa)( a => bindable.map(fb)(b => (a,b)) )

  trait Bindable[F[_]] {
    def map[A, B](a: F[A])(fa: A => B): F[B]
    def flatMap[A, B](a: F[A])(f: A => F[B]): F[B]
  }

  implicit val bindableOpt = new Bindable[Option] {
    override def map[A, B](a: Option[A])(fa: A => B): Option[B] = a.map(fa)

    override def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] = a.flatMap(f)
  }

  implicit val bindableList = new Bindable[List] {
    override def map[A, B](a: List[A])(fa: A => B): List[B] = a.map(fa)

    override def flatMap[A, B](a: List[A])(f: A => List[B]): List[B] = a.flatMap(f)
  }
  def main(args: Array[String]): Unit = {
    val opt1 = Option(1)
    val opt2 = Option(2)
    println(tuplef(opt1, opt2))

    val list1 = List(1)
    val list2 = List(2)
    println(tuplef(list1, list2))
  }




  /*def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
    override def map[B](f: A => B): Option[B] = opt.map(f)

    override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
  }

  def listBindable[A](list: List[A]): Bindable[List, A] = ???




  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val r3: Option[(Int, Int)] = tupleBindable(optBindable(optA), optBindable(optB))
  val r4 = println(tupleBindable(listBindable(list1), listBindable(list2)))


  lazy val r1 = println(tuplef(optA, optB))
  lazy val r2 = println(tuplef(list1, list2))*/

}