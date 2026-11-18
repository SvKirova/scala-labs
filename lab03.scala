// Standard for loop implementation of factorial
def calculateByForLoop(n: Int): BigInt = {
  require(n >= 0, "n must be positive")
  var accumulator: BigInt = 1
  for i <- 1 to n do
    accumulator = accumulator * i
  accumulator
}

// Recursive implementation of factorial
// It may throw StackOverflowError - when the recursion depth
// exceeds the maximum stack size
def calculateByRecursion(n: Int): BigInt = {
  require(n >= 0, "n must be positive")
  n match
    case 0 => 1
    case _ => n * calculateByRecursion(n - 1)
}

// Tail-recursive implementation of factorial
def calculateByTailRecursion(n: Int): BigInt = {
  require(n >= 0, "n must be positive")

  @annotation.tailrec
  def fac(i: Int, acc: BigInt): BigInt = {
    if i > n then acc
    else fac(i + 1, acc * i)
  }

  fac(1, 1)
}

// Exercise 1
def sumList(list: List[Int]): Int = {

  @annotation.tailrec
  def sumListHelper(list: List[Int], accumulator: Int): Int = {
    list match
      case Nil => accumulator
      case head :: tail => sumListHelper(tail, accumulator + head)
  }

  sumListHelper(list, 0)
}

// Exercise 2
@annotation.tailrec
def isSortedAscending(list: List[Int]): Boolean = {
  list match
    case Nil | _ :: Nil => true
    case first :: second :: tail =>
      first <= second && isSortedAscending(second :: tail)
}

// Exercise 3
def mergeAlternate[A](list1: List[A], list2: List[A]): List[A] = {
  (list1, list2) match
    case (Nil, _) => list2
    case (_, Nil) => list1
    case (h1 :: t1, h2 :: t2) => h1 :: h2 :: mergeAlternate(t1, t2)
}

// Exercise 4
@annotation.tailrec
def calculateFactorial(n: Int, accumulator: BigInt = 1): BigInt = {
  require(n >= 0, "n must be a positive number")
  n match
    case 0 => accumulator
    case _ => calculateFactorial(n - 1, accumulator * n)
}

// Exercise 5
def applyTwice(f: Int => Int, x: Int): Int = f(f(x))

// Exercise 6
def createMultiplier(factor: Int): Int => Int = (x: Int) => x * factor

// Exercise 7
def squareList(numbers: List[Int]): List[Int] = {
  numbers.map(x => x * x)
}

// Exercise 8 !!!
def createMultiplierRec(n: Int): Int => Int = {
  (x: Int) =>
    @annotation.tailrec
    def innerLoop(acc: Int, count: Int): Int = {
      if count == 0 then acc
      else innerLoop(acc + x, count - 1)
    }

    innerLoop(0, n)
}

// Exercise 9 !!!
def compose[A, B, C](f: A => B, g: B => C): A => C = {
  (x: A) => g(f(x))
}

// Exercise 10 !!!
def splitAtPivot(xs: List[Int], p: Int => Boolean): (List[Int], List[Int]) = {
  @annotation.tailrec
  def loop(remaining: List[Int], before: List[Int]): (List[Int], List[Int]) = {
    remaining match
      case Nil => (before.reverse, Nil)
      case head :: tail =>
        if p(head) then (before.reverse, remaining)
        else loop(tail, head :: before)
  }

  loop(xs, Nil)
}

@main
def main(): Unit = {
  println(calculateByForLoop(3))
  println(calculateByRecursion(4))
  println(calculateByTailRecursion(5))
  println(sumList(List(1, 2, 3, 4)))
  println(isSortedAscending(List(1, 2, 3, 4)))
  println(isSortedAscending(List(1, 3, 2, 4)))
  println(mergeAlternate(List(1, 2, 3), List(9, 10, 11, 12)))
  println(calculateFactorial(4))
  println(applyTwice(x => x * 2, 5))

  val double = createMultiplier(4)
  val triple = createMultiplier(2)
  println(double(5))
  println(triple(5))

  println(squareList(List(3, 4, 5)))

  val multiply = createMultiplierRec(3)
  println(multiply(5))

  val add1 = (x: Int) => x + 1
  val times2 = (x: Int) => x * 2
  val composed1 = compose(add1, times2)
  val composed2 = compose(times2, add1)
  println(composed1(3))
  println(composed2(3))

  val xs = List(2, 4, 7, 8, 9, 12)
  val p = (x: Int) => x > 7
  val split = splitAtPivot(xs, p)
  println(split)
}
