package com.mizhi.fpscala

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // Exercise 3.4, p35
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }
  }

  // Exercise 3.3, p36
  def setHead[A](l: List[A], h: A): List[A] = {
    Cons(h, tail(l))
  }

  // Exercise 3.4, p36
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(tail(l), n - 1)
  }

  // Exercise 3.5, p36
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(tail(l), f) else l
    }
  }

  def dropWhile2[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile2(t, f)
      case _ => l
    }
  }

  def dropWhile3[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile3(t)(f)
      case _ => l
    }
  }

  // Exercise 3.6. p37
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  // Exercise 3.10, p40
  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  // Exercise 3.12, p41
  def reverse[A](as: List[A]) = foldLeft(as, Nil:List[A])((a, b) => Cons(b, a))

  // Exercise 3.13, p41
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))
  }

  // Exercise 3.14, p41
  def append[A](as: List[A], bs: List[A]): List[A] = {
    foldRight2(as, bs)(Cons(_, _))
  }

  // Exercise 3.15, p41
  def concatAll[A](als: List[List[A]]): List[A] = {
    foldLeft(als, Nil:List[A])(append(_, _))
  }

  // Exercise 3.18, p42
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight2(as, Nil:List[B])((a, b) => Cons(f(a), b))
  }

  // Exercise 3.19, p42
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight2(as, Nil:List[A])((a, b) => if (f(a)) Cons(a, b) else b)
  }

  // Exercise 3.20, p42
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight2(as, Nil:List[B])((a, b) => append(f(a), b))
  }

  // Exercise 3.21, p43
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((a) => if (f(a)) List(a) else Nil)
  }

  // Exercise 3.22, p43
  def addLists(as: List[Int], bs:List[Int]): List[Int] = {
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addLists(xs, ys))
    }
  }

  // Exercise 3.23, p43
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }
  }

  // Exercise 3.24, p44
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => false
      case _ => foldLeft(zipWith(sub, sup)(_ == _), true)(_ && _) || hasSubsequence(tail(sup), sub)
    }
  }
}

// Tree exercises starting on p46
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Exercise 3.25, p46
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }
}
