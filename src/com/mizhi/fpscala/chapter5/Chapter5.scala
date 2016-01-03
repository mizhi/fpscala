package com.mizhi.fpscala.chapter5

sealed trait Stream[+A] {
  def headOption: Option[A] = {
    this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }
  }

  // Exercise 5.1, p69
  def toList: List[A] = {
    this match {
      case Empty => List()
      case Cons(h, t) => h() :: t().toList
    }
  }

  // Exercise 5.2, p70
  def take(n: Int): Stream[A] = {
    this match {
      case _ if n == 0 => Empty
      case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
      case Empty => Empty
    }
  }

  // Exercise 5.2, p70
  def drop(n: Int): Stream[A] = {
    this match {
      case s if n == 0 => s
      case Cons(_, t) if n > 0 => t().drop(n-1)
      case Empty => Empty
    }
  }

  // Exercise 5.3, p70
  def takeWhile(f: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) => {
        if (f(h())) Stream.cons(h(), t().takeWhile(f))
        else Empty
      }
    }
  }

  // Exercise 5.4, p71
  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def forAll2(p: A => Boolean): Boolean = {
    this match {
      case Empty => true
      case Cons(h, t) => p(h()) && t().forAll(p)
    }
  }

  // Exercise 5.5, p71
  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else Empty)
  }

  // Exercise 5.6, p71
  def headOption2: Option[A] = {
    foldRight(None: Option[A])((a, b) => Some(a))
  }

  // Exercise 5.7, p72
  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))
  def filter(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)
  def append[B >: A](bs: => Stream[B]): Stream[B] = foldRight(bs)((a, b) => Stream.cons(a, b))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, b) => f(a).append(b))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  // Exercise 5.8, p74
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  // Exercise 5.9, p74
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  // Exercise 5.10, p75
  def fibs: Stream[Int] = {
    def _fibs(a: Int, b: Int): Stream[Int] = Stream.cons(b, _fibs(b, a + b))
    Stream.cons(0, _fibs(0, 1))
  }

  def fibs2: Stream[Int] = {
    def _fibs2(a: Int, b: Int): Stream[Int] = Stream.cons(a, _fibs2(b, a + b))
    _fibs2(0, 1)
  }

  // Exercise 5.11, p75
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
  }
}
