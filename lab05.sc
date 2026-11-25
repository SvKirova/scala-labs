// Exercise 1
lazy val lazyFibonacci: LazyList[Int] = {
  def fib(a: Int, b: Int): LazyList[Int] = {
    a #:: fib(b, a + b)
  }

  fib(1, 1)
}

println(lazyFibonacci.take(10).toList)

// Exercise 2
sealed trait LazyTree
case object Empty extends LazyTree
case class Node(value: Int, leftExpr: () => LazyTree, rightExpr: () => LazyTree) extends LazyTree {
  lazy val left: LazyTree = leftExpr()
  lazy val right: LazyTree = rightExpr()
}

val tree = Node(
  1,
  () => {
    println("Creating left subtree")
    Node(2, () => Empty, () => Empty)
  },
  () => {
    println("Creating right subtree")
    Node(3, () => Empty, () => Empty)
  }
)

println(tree.left)
println(tree.left)
println(tree.right)

// Exercise 3
lazy val lazyPrimes: LazyList[Int] = {
  def sieve(xs: LazyList[Int]): LazyList[Int] = {
    xs.head #:: sieve(xs.tail.filter(_ % xs.head != 0))
  }

  sieve(LazyList.from(2))
}

println(lazyPrimes.take(10).toList)

// Exercise 4
lazy val lazyFibonacciTree: LazyTree = {
  def fibTree(a: Int, b: Int): LazyTree = {
    Node(a, () => fibTree(b, a + b), () => fibTree(b, a + b))
  }

  fibTree(1, 1)
}

def printTree(tree: LazyTree, n: Int, acc: Int = 0): Int = {
  if acc < n then
    tree match {
      case node@Node(v, _, _) => {
        println(v)
        val leftPrinted = printTree(node.left, n, acc + 1)
        printTree(node.right, n, leftPrinted)
      }
      case Empty => acc
    }
  else
    acc
}

printTree(lazyFibonacciTree, 10)

// Exercise 5
def nPrime(n: Int): Int = lazyPrimes.drop(n - 1).head

// Exercise 6
def lazyFactorialTree: LazyTree = {
  def fact(n: Int, acc: Int): LazyTree =
    Node(acc, () => fact(n + 1, acc * (n + 1)), () => fact(n + 1, acc * (n + 1)))

  fact(1, 1)
}

printTree(lazyFactorialTree, 5)

// Exercise 7
def lazyCartesianProduct[A, B](xs: LazyList[A], ys: LazyList[B]): LazyList[(A, B)] = {
  for
    x <- xs
    y <- ys
  yield (x, y)
}

val xs = LazyList(1, 2, 3)
val ys = LazyList("A", "B")
println(lazyCartesianProduct(xs, ys).toList)
