import com.mizhi.fpscala.chapter6._

val sRNG = SimpleRNG(10)
sRNG.nextInt

// Exercise 6.1, p82
RNG.nonNegativeInt(sRNG)

// Exercise 6.2, p83
RNG.double(sRNG)

// Exercise 6.3, p83
RNG.intDouble(sRNG)
RNG.doubleInt(sRNG)
RNG.double3(sRNG)

// Exercise 6.4, p83
RNG.ints(10)(sRNG)

// Exercise 6.5, p85
RNG.double2(sRNG)

// Exercise 6.6, p85
RNG.map2(RNG.int, RNG.int)(_ * _)(sRNG)

// Exercise 6.7, p85
RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(sRNG)
RNG.sequence2(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(sRNG)

RNG.ints(4)(sRNG)
RNG.ints2(4)(sRNG)
RNG.ints3(4)(sRNG)

RNG.ints(4)(FakeRNG(0))
RNG.ints2(4)(FakeRNG(0))
RNG.ints3(4)(FakeRNG(0))

RNG.sequence(List.fill(3)(RNG.int))(sRNG)
RNG.sequence2(List.fill(3)(RNG.int))(sRNG)

// Exercise 6.8, p87
RNG.flatMap(RNG.ints2(2)) {
  x => RNG.unit(x.map(_.toDouble))
}(sRNG)

RNG.sequence2(List.fill(100)(RNG.nonNegativeLessThan(20)))(sRNG)
