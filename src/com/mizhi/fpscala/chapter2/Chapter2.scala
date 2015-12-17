package com.mizhi.fpscala.chapter2

import scala.annotation.tailrec

object Chapter2 {
  def factorial(n: Int): Int = {
    @tailrec
    def _factorial(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else _factorial(n - 1, n * acc)
    }
    _factorial(n, 1)
  }

  // Exercise 2.1, p21
  def fib(n: Int): Int = {
    @tailrec
    def _fib(n: Int, a: Int, b: Int): Int = {
      if (n == 0) a
      else if (n == 1) b
      else _fib(n - 1, b, a + b)
    }
    _fib(n, 0, 1)
  }

  // Exercise 2.2, p24
  @tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.length <= 1 || (ordered(as.head, as.tail.head) && isSorted(as.tail, ordered))
  }

  // Exercise 2.3, p27
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  // Exercise 2.4, p27
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // Exercise 2.5, p27
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
