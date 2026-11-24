// Exercise 2

def sumNumbers(numbers: Array[Int]): Int = {
  var total = 0
  for (number <- numbers) {
    total += number
  }
  total
}

val numbers = Array(1, 2, 3)
println(s"The result is: ${sumNumbers(numbers)}")
