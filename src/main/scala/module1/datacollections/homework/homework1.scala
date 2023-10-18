package module1.datacollections.homework

import module1.datacollections.homework.BallExperiment


object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 10000

    val listOfExperiments: List[BallExperiment] = List.fill(count)(new BallExperiment)

    val resultOfExperiment = listOfExperiments.map(x => x.takeTwoBallsFromBasket)

    val countOfExperiments: Float = resultOfExperiment.count(x => !x._1)

    val countOfPositiveExperiments: Float = resultOfExperiment.count(x => !x._1 && x._2)

    println(countOfPositiveExperiments / countOfExperiments)
  }
}