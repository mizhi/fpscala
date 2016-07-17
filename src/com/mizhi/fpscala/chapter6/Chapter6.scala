package com.mizhi.fpscala.chapter6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  // Exercise 6.1, p82
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (n, nrng) if n == Int.MinValue => nonNegativeInt(nrng)
      case (n, nrng) => (math.abs(n), nrng)
    }
  }

  // Exercise 6.2, p83
  def double(rng: RNG): (Double, RNG) = {
    val (n, nrng) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue.toDouble, nrng)
  }

  // Exercise 6.3, p83
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, nrng) = rng.nextInt
    val (d, nrng2) = double(nrng)
    ((n, d), nrng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n, d), nrng) = intDouble(rng)
    ((d, n), nrng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, nrng) = double(rng)
    val (d2, nrng2) = double(nrng)
    val (d3, nrng3) = double(nrng2)
    ((d1, d2, d3), nrng3)
  }

  // Exercise 6.4, p83
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def _ints(count: Int)(rng: RNG)(accum: List[Int]): (List[Int], RNG) = {
      if (count == 0) {
        (accum.reverse, rng)
      } else {
        val (n, nrng) = rng.nextInt
        _ints(count - 1)(nrng)(n :: accum)
      }
    }
    _ints(count)(rng)(List())
  }

  def ints3(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (l, r) = (count to 1 by -1).foldRight((List.empty[Int], rng): (List[Int], RNG)) {
      (n, res) => {
        val (rn, nrng) = res._2.nextInt
        (rn :: res._1, nrng)
      }
    }
    (l, r)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  val int: Rand[Int] = _.nextInt

  def map[A, B](ra: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (ra2, rng2) = ra(rng)
      (f(ra2), rng2)
    }
  }

  val double2: Rand[Double] = map(nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble)

  // Exercise 6.6, p85
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rnga2) = ra(rng)
      val (b, rngb2) = rb(rnga2)
      (f(a, b), rngb2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 6.7, p85
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]())) {
      (rnga, rngacc) => map2(rnga, rngacc)(_ :: _)
    }
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.reverse.foldLeft(unit(List[A]())) {
      (rngacc, rnga) => map2(rnga, rngacc)(_ :: _)
    }
  }

  def ints2(count: Int): Rand[List[Int]] = sequence2(List.fill(count)(int))
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class FakeRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    (seed.toInt, FakeRNG(seed + 1))
  }
}
