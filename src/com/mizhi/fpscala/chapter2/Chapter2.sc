import com.mizhi.fpscala.chapter2.Chapter2._

factorial(5)

// Exercise 2.1, p21
fib(0) // 0
fib(1) // 1
fib(2) // 1
fib(3) // 2
fib(4) // 3
fib(5) // 5
fib(6) // 8

// Exercise 2.2, p24
isSorted[Int](Array(2,1), (y, z) => y <= z )
isSorted[Int](Array(), (x, y) => x <= y)

// Exercise 2.3, p27
def addToStr(x: Int, y: Int): String = {
  (x + y).toString
}
val addToFiveStr = curry(addToStr)(5)
addToFiveStr(10)

// Exercise 2.4, p27
uncurry(curry(addToStr))

// Exercise 2.5, p27
def f1(x: Int) = 2 * x
def f2(y: Int) = y + 100

compose(f2, f1)(4)

