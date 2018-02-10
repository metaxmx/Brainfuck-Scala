package com.illucit.bf

import com.illucit.bf.IO.{Return, Suspend}

import scala.annotation.tailrec

/**
  * IO Monad.
  */
sealed trait IO[+A] {

  def flatMap[B](f: A => IO[B]): IO[B] = Suspend(() => f(run))

  def map[B](f: A => B): IO[B] = Return(() => f(run))

  @tailrec
  final def run: A =
    this match {
      case Return(a) => a()
      case Suspend(s) => s().run
    }
}

object IO {

  final case class Return[A](a: () => A) extends IO[A]

  final case class Suspend[A](s: () => IO[A]) extends IO[A]

  def pure[A](a: => A): IO[A] = Return(() => a)

  def apply[A](a: => A): IO[A] = pure(a)

}
