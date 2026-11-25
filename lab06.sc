//Exercise 1
def callByValue(x: Int): Int =
  x + x

def callByName(x: => Int): Int =
  x + x

def compute(): Int =
  println("Computing...")
  10

println("Call-by-value:")
println(callByValue(compute()))

println("Call-by-name:")
println(callByName(compute()))

// Exercise 2
def choose[A](condition: Boolean)(ifTrue: => A, ifFalse: => A): A =
  if condition then ifTrue
  else ifFalse

val result1 = choose(10 > 5)(
  {
    println("Evaluating true expression")
    "True"
  },
  {
    println("Evaluating false expression")
    "False"
  }
)

println(result1)

val result2 = choose(2 > 5)(
  {
    println("Evaluating true expression")
    "True"
  },
  {
    println("Evaluating false expression")
    "False"
  }
)

println(result2)

// Exercise 3
def safeDivide(a: Int, b: Int): Either[String, Int] = {
  if b == 0 then
    Left("Division by zero")
  else
    Right(a / b)
}

def calculate(a: Int, b: Int, c: Int): Either[String, Int] =
  for
    first <- safeDivide(a, b)
    second <- safeDivide(first, c)
  yield second

println(calculate(100, 2, 5))
println(calculate(100, 0, 5))
println(calculate(100, 2, 0))

// Exercise 4
import scala.util.Try

def parseNumber(value: String): Try[Int] =
  Try(value.toInt)

def calculateExpression(a: String, b: String, c: String): Try[Int] =
  for
    x <- parseNumber(a)
    y <- parseNumber(b)
    z <- parseNumber(c)
    result <- Try((x + y) / z)
  yield result

println(calculateExpression("10", "20", "2"))
println(calculateExpression("10", "abc", "2"))
println(calculateExpression("10", "20", "0"))

// Exercise 5
def transformArray(numbers: Array[Int]): Unit =
  var i = 0

  while i < numbers.length do
    if numbers(i) > 0 then
      numbers(i) = numbers(i) * 2
    else if numbers(i) < 0 then
      numbers(i) = 0

    i += 1

val numbers = Array(1, -2, 3, 0, -5, 6)

println(s"Before: ${numbers.toList}")

transformArray(numbers)

println(s"After: ${numbers.toList}")

// Exercise 6
def matrixStatistics(matrix: Array[Array[Int]]): (Int, Int, Int) = {
  var sum = 0
  var evenCount = 0
  var max = matrix(0)(0)
  var i = 0

  while i < matrix.length do
    var j = 0

    while j < matrix(i).length do
      val value = matrix(i)(j)

      sum += value

      if value % 2 == 0 then
        evenCount += 1

      if value > max then
        max = value

      j += 1

    i += 1

  (sum, evenCount, max)
}

val matrix = Array(
  Array(1, 2, 3),
  Array(4, 5, 6),
  Array(7, 8, 9)
)

val (sum, evenCount, max) = matrixStatistics(matrix)

println(s"Sum: $sum")
println(s"Even numbers: $evenCount")
println(s"Maximum: $max")