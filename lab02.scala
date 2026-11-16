// Example factorial func with recursion
def factorial(n: Int): BigInt =
  if n == 0 then 1
  else n * factorial(n - 1)

// Example factorial func with for loop
def factorialFor(n: Int): BigInt = {
  require(n >= 0, "n must be non-negative")
  var accumulator: BigInt = 1
  for i <- 1 to n do
    accumulator = i * accumulator
  accumulator
}

// Example in functional way by recursion
def calculateByRecursion(n: Int): BigInt = {
  require(n >= 0, "n must be non-negative")
  n match
    case 0 => 1
    case _ => n * calculateByRecursion(n - 1)
}

// Exercise 1
def sumList(list: List[Int]): Int = {
  var sum = 0
  for (i <- list) {
    if (i % 2 != 0) {
      sum += i
    }
  }
  sum
}

// Exercise 2
def sumListRecursive(list: List[Int]): Int = {
  list match
    case Nil => 0
    case head :: tail =>
      if head % 2 != 0 then head + sumListRecursive(tail)
      else sumListRecursive(tail)
}

// Exercise 3
// by teacher
def connectStrings(listOfStrings: List[String], separator: String): String = {
  listOfStrings.mkString(separator)
}

def connectStrings2(listOfStrings: List[String], separator: String): String = {
  listOfStrings match
    case Nil => ""
    case head :: Nil => head
    case head :: tail => head + separator + connectStrings2(tail, separator)
}

// Exercise 4
def fibonacci(n: Int): BigInt = {
  n match
    case 0 => 0
    case 1 => 1
    case _ => fibonacci(n - 1) + fibonacci(n - 2)
}

// by teacher
def fibonacci2(n: Int): BigInt = {
  if (n == 0) {
    0
  } else if (n == 1) {
    1
  } else {
    fibonacci(n - 1) + fibonacci(n - 2)
  }
}

// Exercise 5
def mList(list: List[Int]): Int = {
  var acc = 1
  for element <- list if element % 2 != 0 do acc *= element
  acc
}

def mList2(list: List[Int]): Int = {
  var acc = 1
  for (element <- list) {
    if (element % 2 != 0) then
      acc *= element
  }
  acc
}

// by teacher
def mList3(list: List[Int]): Int = {
  var product = 1
  var hasOdd = false
  for (x <- list) do
    if (x % 2 != 0) {
      product *= x
      hasOdd = true
    }
  product
}

// Exercise 6
def mListRec(list: List[Int]): Int = {
  list match
    case Nil => 1
    case head :: tail =>
      if head % 2 != 0 then head * mListRec(tail)
      else mListRec(tail)
}

// by teacher
def multiplyOddRec(list: List[Int]): Int = {
  list match
    case Nil => 1
    case x :: xs =>
      if x % 2 != 0 then x * multiplyOddRec(xs)
      else multiplyOddRec(xs)
}

// Exercise 7
def sumWholeList(list: List[Int]): Int = {
  var sum = 0
  for (element <- list) {
    sum += element
  }
  sum
}

def sumWholeList2(list: List[Int]): Int = {
  list.sum
}

// by teacher
def sumListEx7(list: List[Int]): Int = {
  var sum = 0
  for (i <- list) do sum += i
  sum
}

// Exercise 8
def sumEven(list: List[Int]): Int = {
  list match
    case Nil => 0
    case head :: tail =>
      if (head % 2 == 0) then head + sumEven(tail)
      else sumEven(tail)
}

// by teacher
def sumListEx8(list: List[Int]): Int = {
  list match
    case Nil => 0
    case x :: xs =>
      if x % 2 == 0 then x + sumListEx8(xs)
      else sumListEx8(xs)
}

// Exercise 9
def func(n: Int): Int = {
  if (n <= 1) {
    -2
  } else {
    (n - 1) * func(n - 1) + 3
  }
}

// by teacher
def funcEx9(n: Int): Int = {
  if (n <= 1) {
    -2
  } else {
    (n - 1) * funcEx9(n - 1) + 3
  }
}

@main
def main(): Unit = {
  // Example factorial func with recursion
  println(factorial(13))

  // Example factorial func with for loop
  println(factorialFor(13))

  // Exercise 1
  val list = List(1, 2, 3)
  println(sumList(list))

  // Exercise 2
  val list2 = List(1, 2, 3, 4, 5)
  println(sumListRecursive(list2))

  // Exercise 3
  val stringsList = List("Didi", "Kali", "Roni")
  val separator = "/"
  println(connectStrings(stringsList, separator))
  println(connectStrings2(stringsList, separator))

  // Exercise 4
  println(fibonacci(7))
  println(fibonacci2(6))

  // Exercise 5
  val listM = List(3, 5, 10)
  println(mList(listM))

  // Exercise 6
  println(mListRec(listM))
  println(multiplyOddRec(listM))

  // Exercise 7
  val listToSum = List(2, 3, 4, 5)
  println(sumWholeList(listToSum))

  // Exercise 8
  val listEven = List(2, 5, 4, 2)
  println(sumEven(listEven))

  // Exercise 9
  println(func(5))
}

