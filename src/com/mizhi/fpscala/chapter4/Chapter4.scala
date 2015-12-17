package com.mizhi.fpscala.chapter4

import scala.collection.immutable.List

// Exercise 4.1, p54
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

case class Some[+A](value: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(value))
  override def flatMap[B](f: A => Option[B]) = f(value)
  override def getOrElse[B >: A](default: => B): B = value
  override def orElse[B >: A](ob: => Option[B]): Option[B] = this
  override def filter(f: A => Boolean): Option[A] = if (f(value)) this else None
}

case object None extends Option[Nothing] {
  override def map[B](f: (Nothing) => B): Option[B] = None
  override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None
  override def getOrElse[B >: Nothing](default: => B): B = default
  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
  override def filter(f: (Nothing) => Boolean): Option[Nothing] = None
}

object Option {
  // using type inference
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  // being explicit
  def lift2[A,B](f: A => B): Option[A] => Option[B] = (a: Option[A]) => a map f

  // Exercise 4.3, p58
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  // Exercise 4.4, p59
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case None :: tail => None
      case Some(head) :: tail => sequence(tail).map(head :: _)
    }
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(List()))(
      (aa: Option[A], bb: Option[List[A]]) => aa.flatMap(aaa => bb.map(bbb => aaa :: bbb))
    )
  }

  def sequence3[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((aa, bb) => map2(aa, bb)(_ :: _))
  }
}
