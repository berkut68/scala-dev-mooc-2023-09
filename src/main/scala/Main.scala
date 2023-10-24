import module1.threads.{Foo, Thread1, ToyFuture, getRatesLocation1, getRatesLocation2, getRatesLocation3, getRatesLocation4, getRatesLocation5, getRatesLocation6, getRatesLocation7, getRatesLocation8, printRunningTime}

object Main {

  def main(args: Array[String]): Unit = {
    println(s"Hello world " +
      s"[${Thread.currentThread().getName}]")

//    val t1 = new Thread{
//      override def run(): Unit = {
//        Thread.sleep(1000)
//        println(s"Hello from " +
//          s"[${Thread.currentThread().getName}]")
//      }
//    }
//    val t2 = new Thread1
//    t1.start()
//    t2.start()
//    t1.join()

    def rates = {
      val start = System.currentTimeMillis()

      getRatesLocation7.onComplete{ i1 =>
        getRatesLocation8.onComplete{ i2 =>
          println(i1 + i2)
          val end = System.currentTimeMillis()
          println(s"Execution time ${end - start}")
        }
      }
    }

    val result: ToyFuture[Int] = for{
      i1 <- getRatesLocation7
      i2 <- getRatesLocation8
    } yield i1 + i2


    printRunningTime(rates)

  }
}