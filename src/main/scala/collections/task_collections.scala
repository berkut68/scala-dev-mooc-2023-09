package collections

object task_collections {

  def isASCIIString(str: String): Boolean = str.matches("[A-Za-z]+")

  /**
   * Реализуйте метод который первый элемент списка не изменяет, а для последующих алгоритм следующий:
   * если isASCIIString is TRUE тогда пусть каждый элемент строки будет в ВЕРХНЕМ регистре
   * если isASCIIString is FALSE тогда пусть каждый элемент строки будет в нижнем регистре
   * Пример:
   * capitalizeIgnoringASCII(List("Lorem", "ipsum" ,"dolor", "sit", "amet")) -> List("Lorem", "IPSUM", "DOLOR", "SIT", "AMET")
   * capitalizeIgnoringASCII(List("Оказывается", "," "ЗвУк", "КЛАВИШЬ", "печатной", "Машинки", "не", "СТАЛ", "ограничивающим", "фактором")) ->
   * List("Оказывается", "," "звук", "КЛАВИШЬ", "печатной", "машинки", "не", "стал", "ограничивающим", "фактором")
   * HINT: Тут удобно использовать collect и zipWithIndex
   *
   * **/
  def capitalizeIgnoringASCII(text: List[String]): List[String] = {
    val changeElement: PartialFunction[(String, Int), String] = new PartialFunction[(String, Int), String] {
      override def isDefinedAt(x: (String, Int)): Boolean = x._2 != 0

      override def apply(v1: (String, Int)): String = if (isASCIIString(v1._1)) v1._1.toUpperCase() else v1._1.toLowerCase()
    }

    List(text.head) ++ text.zipWithIndex.collect(changeElement)
  }
  /**
   *
   * Компьютер сгенерировал текст используя вместо прописных чисел, числа в виде цифр, помогите компьютеру заменить цифры на числа
   * В тексте встречаются числа от 0 до 9
   *
   * Реализуйте метод который цифровые значения в строке заменяет на числа: 1 -> one, 2 -> two
   *
   * HINT: Для всех возможных комбинаций чисел стоит использовать Map
   * **/
  def numbersToNumericString(text: String): String = {
    val dictionary =
      Map("0" -> "zero", "1" -> "one", "2" -> "two", "3" -> "thee", "4" -> "four"
        , "5" -> "five", "6" -> "six", "7" -> "seven", "8" -> "eight", "9" -> "nine")

    text.split(" ").map(x => dictionary.getOrElse(x, x)).mkString(" ")
  }

  /**
   *
   * У нас есть два дилера со списками машин которые они обслуживают и продают (case class Auto(mark: String, model: String)).
   * Базы данных дилеров содержат тысячи и больше записей. Нет гарантии что записи уникальные и не имеют повторений
   * HINT: Set
   * HINT2: Iterable стоит изменить
   * **/

  case class Auto(mark: String, model: String)

  /**
   * Хотим узнать какие машины можно обслужить учитывая этих двух дилеров
   * Реализуйте метод который примет две коллекции (два источника) и вернёт объединенный список уникальный значений
   **/
  def intersectionAuto(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
    val result =
      for (
        car1 <- dealerOne;
        car2 <- dealerTwo;
        if car1 != car2
      ) yield {
        Vector(car1, car2)
      }

    result.flatten.toSet
  }

  /**
   * Хотим узнать какие машины обслуживается в первом дилеромском центре, но не обслуживаются во втором
   * Реализуйте метод который примет две коллекции (два источника)
   * и вернёт уникальный список машин обслуживающихся в первом дилерском центре и не обслуживающимся во втором
   **/
  def filterAllLeftDealerAutoWithoutRight(dealerOne: Seq[Auto], dealerTwo: Seq[Auto]): Iterable[Auto] = {
    val result =
      for (
        car1 <- dealerOne;
        car2 <- dealerTwo
        if !dealerTwo.contains(car1)
      ) yield {
        (car1, car2)
      }

    result.map(x => x._1).toSet
  }

}