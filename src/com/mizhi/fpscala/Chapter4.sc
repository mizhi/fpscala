import com.mizhi.fpscala._

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
