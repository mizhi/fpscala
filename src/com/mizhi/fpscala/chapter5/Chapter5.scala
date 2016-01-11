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

  // Exercise 5.13, p75
  def map2[B](f: A => B): Stream[B] = {
    Stream.unfold(this) {
      case Empty => None
      case Cons(hd, tl) => Some((f(hd()), tl()))
    }
  }

  def take2(n: Int): Stream[A] = {
    Stream.unfold((this, n)) {
      case (Cons(hd, tl), _n) if _n > 0 => Some((hd(), (tl(), _n - 1)))
      case _ => None
    }
  }

  def takeWhile3(p: A => Boolean): Stream[A] = {
    Stream.unfold(this) {
      case Cons(hd, tl) if p(hd()) => Some((hd(), tl()))
      case _ => None
    }
  }

  def zipWith[B,C](bs: Stream[B])(f: (A, B) => C): Stream[C] = {
    Stream.unfold((this, bs)) {
      case (_, Empty) => None
      case (Empty, _) => None
      case (Cons(ahd, atl), Cons(bhd, btl)) =>
        Some((f(ahd(), bhd()), (atl(), btl())))
    }
  }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold((this, bs)) {
      case (Cons(ahd, atl), Empty) => Some(((Some(ahd()), None), (atl(), Empty)))
      case (Empty, Cons(bhd, btl)) => Some((None, Some(bhd())), (Empty, btl()))
      case (Cons(ahd, atl), Cons(bhd, btl)) => Some((Some(ahd()), Some(bhd())), (atl(), btl()))
      case _ => None
    }
  }

  // 5.14, p76
  def startsWith[A](bs: Stream[A]): Boolean = {
    zipAll(bs).takeWhile {
      case (Some(a), Some(b)) => true
      case (None, Some(b)) => true
      case (_, None) => false
    }.forAll(p => p._1 == p._2)
  }

  def startsWith2[A](bs: Stream[A]): Boolean = {
    zipAll(bs).takeWhile {
      case (_, Some(b)) => true
      case (_, None) => false
    }.forAll(p => p._1 == p._2)
  }

  def startsWith3[A](bs: Stream[A]): Boolean = {
    zipAll(bs).takeWhile(!_._2.isEmpty).forAll(p => p._1 == p._2)
  }

  // 5.15, p76
  def tails: Stream[Stream[A]] = {
    Stream.unfold(this) {
      a => a match {
        case Cons(hd, tl) => Some(a, tl())
        case Empty => None
      }
    }.append(Stream(Empty))
  }

  def tails2: Stream[Stream[A]] = {
    Stream.unfold(this) {
      case Empty => None
      case s => Some(s, s.drop(1))
    }.append(Stream(Empty))
  }
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

  // Exercise 5.12, p75
  def ufones: Stream[Int] = unfold(())(_ => Some((1, ())))
  def ufconstant[A](a: A): Stream[A] = unfold(())(_ => Some((a, ())))
  def uffrom(n: Int): Stream[Int] = unfold(n)(from => Some((from, from + 1)))
  def uffibs: Stream[Int] = unfold((0, 1))(ab => Some((ab._1, (ab._2, ab._1 + ab._2))))
}
