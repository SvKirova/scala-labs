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

// Exercise 2
sealed trait TreeInt

case object Empty extends TreeInt
case class NodeInt(value: Int, left: TreeInt = Empty, right: TreeInt = Empty) extends TreeInt

def sumTreeInt(tree: TreeInt): Int = {
  tree match
    case Empty => 0
    case NodeInt(value, left, right) => value + sumTreeInt(left) + sumTreeInt(right)
}

// Exercise 3
// TO BE DONE

@main
def main(): Unit = {
  val trafficLight = trafficAction(Red)
  println(trafficLight)

  val tree = NodeInt(5, NodeInt(2), NodeInt(7, NodeInt(8)))
  println(sumTreeInt(tree))
}

