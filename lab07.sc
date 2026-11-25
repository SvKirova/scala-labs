// Exercise 1
def safeDivide(a: Int, b: Int): Option[Int] = {
  b match
    case 0 => None
    case _ => Some(a / b)
}

println(safeDivide(10, 2))
println(safeDivide(10, 0))

// Exercise 2
lazy val squareOf10: Int = 10 * 10
println(squareOf10)

// Exercise 3
import scala.util.Try

def parseInt(string: String): Try[Int] = {
  Try(string.toInt)
}

println(parseInt("Hello"))
println(parseInt("5"))

// Exercise 4
def applyAndPrint[A, B](x: A, f: A => B): Unit = {
  println(f(x))
}

applyAndPrint(5, _ * 2)
applyAndPrint("hello", _.toUpperCase)

// Exercise 5
// in build.sbt:
// libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"
// libraryDependencies += "org.typelevel" %% "cats-effect" % "3.7.1"

import cats.data.State

def increment: State[Int, Int] = {
  State(s => (s + 1, s + 1))
}

val (newState, result) = increment.run(10).value
println(s"State after increment: $newState, result: $result")

// Exercise 6
type Counter[A] = State[Int, A]
val inc: Counter[Int] = State(s => (s + 1, s + 1))
val dec: Counter[Int] = State(s => (s - 1, s - 1))
val read: Counter[Int] = State(s => (s, s))

val program = for
  a <- inc
  b <- inc
  _ <- dec
  c <- read
yield c

println(program.run(0).value)

// Exercise 7
import cats.data.Validated
import cats.data.Validated.{Valid, Invalid}
import cats.syntax.apply.*

def validateAge(age: Int): Validated[List[String], Int] = {
  age match
    case n if n >= 0 => Valid(n)
    case _ => Invalid(List("Age must be non-negative"))
}

def validateEmail(email: String): Validated[List[String], String] = {
  email match
    case e if e.contains("@") => Valid(e)
    case _ => Invalid(List("Email must contain @"))
}

def validateUser(age: Int, email: String) =
  (validateAge(age), validateEmail(email)).mapN((a, e) => (a, e))

println(validateUser(20, "test@example.com"))
println(validateUser(-5, "invalid"))