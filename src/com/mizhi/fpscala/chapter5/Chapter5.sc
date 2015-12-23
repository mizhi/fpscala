import com.mizhi.fpscala.chapter5._

val si = Stream(1,2,3,4,5)

// Exercise 5.1, p69
si.toList

// Exercise 5.2, p70
si.take(2).toList

// Exercise 5.2, p70
si.drop(2).toList

// Exercise 5.3, p70
si.takeWhile(_ < 4).toList


