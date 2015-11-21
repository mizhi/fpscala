package com.mizhi.fpscala

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

  def fib(n: Int): Int = {
    @tailrec
    def _fib(n: Int, a: Int, b: Int): Int = {
      if (n == 0) a
      else if (n == 1) b
      else _fib(n - 1, b, a + b)
    }
    _fib(n, 0, 1)
  }

  @tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.length <= 1 || (ordered(as.head, as.tail.head) && isSorted(as.tail, ordered))
  }
}
