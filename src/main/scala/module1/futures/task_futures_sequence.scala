package module1.futures

import module1.futures.HomeworksUtils.TaskSyntax


import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */


  /*def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
    task"Реализуйте метод `fullSequence`" ()*/

  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    futures.foldLeft(Future((List.empty[A], List.empty[Throwable]))) {
      (acc, f) => {
        acc.flatMap{
          case (list1, list2) => {
            f.map(value => (list1 :+ value, list2))
              .recoverWith{
                case Throwable @ ex => Future.successful((list1, list2 :+ ex))
              }
          }
        }
      }
    }
  }

}
