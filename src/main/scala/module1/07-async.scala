package module1

import module1.utils.NameableThreads

import java.util.concurrent.{Executor, ExecutorService, Executors}
import scala.collection.mutable
import scala.language.{existentials, postfixOps}

object threads {


  // Thread

  class Thread1 extends Thread{
    override def run(): Unit = {
      println(s"Hello from [${Thread.currentThread().getName}]")
    }
  }



  def printRunningTime(v: => Unit): Unit = {
    val start = System.currentTimeMillis()
    v
    val end = System.currentTimeMillis()
    println(s"Execution time ${end - start}")
  }

  // rates

  def getRatesLocation1 = {
    Thread.sleep(1000)
    println("GetRatesLocation1")
  }

  def getRatesLocation2 = {
    Thread.sleep(2000)
    println("GetRatesLocation2")
  }


  // async

  def async(f: => Unit): Thread = new Thread{
    override def run(): Unit = f
  }

  def getRatesLocation3 = async{
    Thread.sleep(1000)
    println("GetRatesLocation1")
  }

  def getRatesLocation4 = async{
    Thread.sleep(2000)
    println("GetRatesLocation2")
  }

  def async2[A](f: => A): A = {
    var r: A = null.asInstanceOf[A]
    val t = new Thread{
      override def run(): Unit = r = f
    }
    t.start()
    t.join()
    r
  }

  def getRatesLocation5 = async2{
    Thread.sleep(1000)
    println("GetRatesLocation1")
    10
  }

  def getRatesLocation6 = async2{
    Thread.sleep(2000)
    println("GetRatesLocation2")
    20
  }


  class Foo[T](v: => T){
    v
  }

  class ToyFuture[T] private(v: => T){

    private var isCompleted: Boolean = false
    private var r: T = null.asInstanceOf[T]
    private val q = mutable.Queue[T => _]()

    def map[B](f: T => B): ToyFuture[B] = ???
    def flatMap[B](f: T => ToyFuture[B]): ToyFuture[B] = ???


    def onComplete[U](f: T => U): Unit = {
        if(isCompleted) f(r)
        else q.enqueue(f)
    }

    private def start(executor: Executor) = {
      val t = new Runnable {
        override def run(): Unit = {
          r = v
          isCompleted = true
          while (q.nonEmpty){
            q.dequeue()(r)
          }
        }
      }
      executor.execute(t)
    }
  }

  object ToyFuture{
    def apply[T](v: => T)(executor: Executor): ToyFuture[T] = {
      val f = new ToyFuture[T](v)
      f.start(executor)
      f
    }
  }


  val getRatesLocation7: ToyFuture[Int] = ToyFuture{
    Thread.sleep(1000)
    println("GetRatesLocation1")
    10
  }(executor.pool1)

  val getRatesLocation8: ToyFuture[Int] = ToyFuture{
    Thread.sleep(2000)
    println("GetRatesLocation2")
    20
  }(executor.pool1)


}

object executor {
      val pool1: ExecutorService =
        Executors.newFixedThreadPool(2, NameableThreads("fixed-pool-1"))
      val pool2: ExecutorService =
        Executors.newCachedThreadPool(NameableThreads("cached-pool-2"))
      val pool3: ExecutorService =
        Executors.newWorkStealingPool(4)
      val pool4: ExecutorService =
        Executors.newSingleThreadExecutor(NameableThreads("singleThread-pool-4"))
}