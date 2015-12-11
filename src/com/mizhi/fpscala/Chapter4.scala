package com.mizhi.fpscala


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

