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

// Exercise 5.4, p71
si.forAll(_ % 2 == 0)
si.forAll(_ < 10)

si.forAll2(_ % 2 == 0)

// Exercise 5.5, p71
si.takeWhile2(_ < 3).toList

// Exercise 5.6, p71
si.headOption2
Empty.headOption2

// Exercise 5.7, p72
si.map(_ * 2).toList
si.filter(_ % 2 == 0).toList
si.append(Stream(9,8,7)).toList
si.flatMap(a => Stream(a * 2)).toList

val ones: Stream[Int] = Stream.cons(1, ones)
ones.take(10).toList

// Exercise 5.8, p74
val as = Stream.constant('A')
as.take(10).toList

// Exercise 5.9, p74
val from10 = Stream.from(10)
from10.take(10).toList

// Exercise 5.10, p75
Stream.fibs.take(10).toList
Stream.fibs2.take(10).toList

// Exercise 5.12, p75
Stream.ufones.take(10).toList
Stream.ufconstant('A').take(10).toList
Stream.uffrom(10).take(10).toList
Stream.uffibs.take(10).toList

// Exercise 5.13, p76
si.map2(_ * 2).toList
Stream.ufones.take2(10).toList
Stream.ufones.take2(1).toList
Stream.ufones.take(0).toList
si.takeWhile3(_ < 3).toList

val si2 = Stream('A', 'B', 'C')

si.zipWith(si2)( (n, c) => (n, c)).toList

si.zipAll(si2).toList

// 5.14, p76
val s3 = Stream(1,2,3)
val s4 = Stream(1,2,3,4,5)
s3.startsWith(s4)
s4.startsWith(s3)

// 5.15, p76
s3.tails.map(_.toList).toList
s3.tails2.map(_.toList).toList
