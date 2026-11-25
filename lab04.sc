// Exercise 1
sealed trait TrafficLight

case object Red extends TrafficLight
case object Yellow extends TrafficLight
case object Green extends TrafficLight

// or it can be with enum
enum TrafficLight2:
  case Red, Yellow, Green

def trafficAction(light: TrafficLight): String = {
  light match
    case Red => "Stop"
    case Yellow => "Slow"
    case Green => "Go"
}

val trafficLight = trafficAction(Red)
println(trafficLight)

// Exercise 2
sealed trait TreeInt
case object Empty extends TreeInt
case class NodeInt(value: Int, left: TreeInt = Empty, right: TreeInt = Empty) extends TreeInt

def sumTreeInt(tree: TreeInt): Int = {
  tree match
    case Empty => 0
    case NodeInt(value, left, right) => value + sumTreeInt(left) + sumTreeInt(right)
}

val tree = NodeInt(5, NodeInt(2), NodeInt(7, NodeInt(8)))
println(sumTreeInt(tree))

// Exercise 3
sealed trait Tree[+A]
case object EmptyTree extends Tree[Nothing]
case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

def maxDepth[A](t: Tree[A]):Int = {
  t match
    case EmptyTree => 0
    case Node(_, lt, rt) => 1 + (maxDepth(lt) max maxDepth(rt))
}

def isBalanced[A](t: Tree[A]): Boolean = {
  t match
    case EmptyTree => true
    case Node(_, lt, rt) => {
      val difference = maxDepth(lt) - maxDepth(rt)
      difference.abs <= 1 && isBalanced(lt) && isBalanced(rt)
    }
}

def transformTree[A, B](t:Tree[A], f:A=>B): Tree[B] = {
  t match
    case EmptyTree => EmptyTree
    case Node(v, lt, rt) => Node(f(v), transformTree(lt, f), transformTree(rt, f))
}

val tree2 = Node(1, Node(2, EmptyTree, Node(3, EmptyTree, Node(4, EmptyTree, EmptyTree))), EmptyTree)
println(transformTree(tree2, _*10))
