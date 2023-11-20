package scala3_2

import scala3_2.homework3.Logarithm


object homework1 {
  extension (x: String) {
    def ++(b: String): Int = x.toInt + b.toInt
  }
  @main def part1Ex(): Unit = {
    println("1" ++ "33")
  }
}

object homework2 {
  enum CompletionArg {
    case StringWrapper(a: String)
    case IntWrapper(a: Int)
    case FloatWrapper(a: Float)
  }

  object CompletionArg {
    given fromString: Conversion[String, CompletionArg] = StringWrapper(_)
    given fromInt: Conversion[Int, CompletionArg] = IntWrapper(_)
    given fromFloat: Conversion[Float, CompletionArg] = FloatWrapper(_)
  }
  def complete(input: CompletionArg) =
  import CompletionArg._
  input match {
    case StringWrapper(a) => a
    case IntWrapper(a) => a
    case FloatWrapper(a) => a
  }

  @main def part2Ex(): Unit ={
    println(complete("String"))
    println(complete(1))
    println(complete(7f))
  }
}


object homework3 {
  opaque type Logarithm = Double

  object Logarithm {
    def apply(d: Double): Logarithm = math.log(d)
  }

  extension (x: Logarithm)
  def toDouble(): Logarithm = math.exp(x)
  def +(y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
  def *(y: Logarithm): Logarithm = x * y
  @main def part3Ex(): Unit = {
    import Logarithm._

    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)
    val l3 = l * l2
    val l4 = l + l2

    println(l)
    println(l2)
    println(l3)
    println(l4)
  }
}
