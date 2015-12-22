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
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch { case e: Exception => None }
  }

  // using type inference
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  // being explicit
  def lift2[A,B](f: A => B): Option[A] => Option[B] = (a: Option[A]) => a map f

  // Exercise 4.3, p58
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  // Exercise 4.4, p59
  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    as match {
      case Nil => Some(Nil)
      case None :: tail => None
      case Some(head) :: tail => sequence(tail).map(head :: _)
    }
  }

  def sequence2[A](as: List[Option[A]]): Option[List[A]] = {
    as.foldRight[Option[List[A]]](Some(List()))(
      (aa: Option[A], bb: Option[List[A]]) => aa.flatMap(aaa => bb.map(bbb => aaa :: bbb))
    )
  }

  def sequence3[A](as: List[Option[A]]): Option[List[A]] = {
    as.foldRight[Option[List[A]]](Some(Nil))((aa, bb) => map2(aa, bb)(_ :: _))
  }

  // Exercise 4.5, p59
  def traverse[A, B](aa: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequence3(aa.map(f))
  }

  def traverse2[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    as match {
      case Nil => Some(Nil)
      case head :: tail => map2(f(head), traverse2(tail)(f))(_ :: _)
    }
  }

  def sequence4[A](as: List[Option[A]]): Option[List[A]] = {
    traverse2(as)(a => a)
  }
}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(b) => Right(b)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => b.map(f(a, _))
    }
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  // Exercise 4.7, p62
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(e => e)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }
  }
}
