package module1.datacollections.homework

import module1.datacollections.homework.BallExperiment.mapIntToBall

import scala.util.Random

sealed trait Ball
case object WhiteBall extends Ball
case object BlackBall extends Ball

class BallExperiment {
  private val WhiteBallCount = 3
  private val BlackBallCount = 3
  private var totalBallCount = WhiteBallCount + BlackBallCount
  class Basket(val whiteBallCount: Int = WhiteBallCount, val blackBallCount: Int = BlackBallCount) {
    var balls: List[Ball] = {
      // "взболтаем" корзину
      Random.shuffle(
        (1 to whiteBallCount).map(_ => mapIntToBall(1)).toList
          ++ (1 to blackBallCount).map(_ => mapIntToBall(0)).toList)
    }
  }

  private val basket = new Basket()

  def takeTwoBallsFromBasket: (Boolean, Boolean) = {
    (isWhiteBallPicked(takeBallFromBasket()), isWhiteBallPicked(takeBallFromBasket()))
  }
  private def takeBallFromBasket(): Ball = {
    //println("Before taking ball: " + basket.balls)

    val ballNumber = scala.util.Random.nextInt(totalBallCount)

    val ball = this.basket.balls(ballNumber)

    //println(s"[INFO] ${ballNumber}th ball has been picked: $ball, ${isWhiteBallPicked(ball)}")

    basket.balls = basket.balls.take(ballNumber) ++ basket.balls.takeRight((totalBallCount - 1) - ballNumber)

    //println(s"After taking ${ballNumber} ball: " + basket.balls)

    totalBallCount = totalBallCount - 1

    ball
  }

  private def isWhiteBallPicked(ball: Ball): Boolean = {
    ball match {
      case WhiteBall => true
      case BlackBall => false
    }
  }
}

object BallExperiment {
  def mapIntToBall(a: Int): Ball = {
    a match {
      case 1 => WhiteBall
      case 0 => BlackBall
    }
  }
}
