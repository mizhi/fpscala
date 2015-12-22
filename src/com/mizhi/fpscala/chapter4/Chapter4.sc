import com.mizhi.fpscala.chapter4._
// For later problems
def mean(xs: Seq[Double]): Option[Double] = {
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)
}
// Exercise 4.2, p55
def variance(xs: Seq[Double]): Option[Double] = {
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}
variance(Seq(1.0,1.0,1.0,1.0))
variance(Seq(1.0,2.0,3.0,4.0))
// Exercise 4.4, p59
val l = List(Some(1), Some(2), Some(3))
val l2 = List(Some(1), None, Some(3))
Option.sequence(l)
Option.sequence(l2)
Option.sequence(Nil)
Option.sequence2(l)
Option.sequence2(l2)
Option.sequence2(Nil)

// Exercise 4.6, p62
val right = Right(42)
val left = Left(new RuntimeException("FOO"))
right.map(2 * _)
left.map(println _)

// Exercise 4.7, p62
val rightl = List(Right(42), Right(10), Right(20))
Either.sequence(rightl)

val leftl = List(Right(42), Left(new RuntimeException("foo")), Right(20))
Either.sequence(leftl)

