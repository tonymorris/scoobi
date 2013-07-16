package com.nicta.scoobi
package core

trait ScoobiConfiguration2 {
  val concurrentJobs: Option[Boolean]
  val uploadedLibJars: Option[Boolean]

}

object ScoobiConfiguration2 {
  private[core] def apply(): ScoobiConfiguration2 =
    ???
}

sealed trait ScoobiConfiguration2Reader[A] {
  val read: ScoobiConfiguration2 => A

  import ScoobiConfiguration2Reader._

  def apply(c: ScoobiConfiguration2): A =
    read(c)

  def map[B](f: A => B): ScoobiConfiguration2Reader[B] =
    ScoobiConfiguration2Reader(f compose read)

  def flatMap[B](f: A => ScoobiConfiguration2Reader[B]): ScoobiConfiguration2Reader[B] =
    ScoobiConfiguration2Reader(c => f(read(c))(c))

  def state(f: ScoobiConfiguration2Reader[ScoobiConfiguration2]): ScoobiConfiguration2State[A] =
    ScoobiConfiguration2State(c => (read(c), f(c)))

  def liftState: ScoobiConfiguration2State[A] =
    state(get)

  def withConfiguration(f: ScoobiConfiguration2Reader[ScoobiConfiguration2]): ScoobiConfiguration2Reader[A] =
    ScoobiConfiguration2Reader(c => read(f(c)))
}

object ScoobiConfiguration2Reader {
  def apply[A](r: ScoobiConfiguration2 => A): ScoobiConfiguration2Reader[A] =
    new ScoobiConfiguration2Reader[A] {
      val read = r
    }

  def value[A](a: => A): ScoobiConfiguration2Reader[A] =
    apply(_ => a)

  val get: ScoobiConfiguration2Reader[ScoobiConfiguration2] =
    apply(identity)
}

sealed trait ScoobiConfiguration2State[A] {
  val read: ScoobiConfiguration2 => (A, ScoobiConfiguration2)

  def apply(c: ScoobiConfiguration2): (A, ScoobiConfiguration2) =
    read(c)

  def eval(c: ScoobiConfiguration2): A =
    apply(c)._1

  def evalReader: ScoobiConfiguration2Reader[A] =
    ScoobiConfiguration2Reader(apply(_)._1)

  def exec(c: ScoobiConfiguration2): ScoobiConfiguration2 =
    apply(c)._2

  def map[B](f: A => B): ScoobiConfiguration2State[B] =
    ScoobiConfiguration2State(c => {
      val (a, d) = read(c)
      (f(a), d)
    })

  def flatMap[B](f: A => ScoobiConfiguration2State[B]): ScoobiConfiguration2State[B] =
    ScoobiConfiguration2State(c => {
      val (a, d) = read(c)
      f(a)(d)
    })

  def withConfiguration(f: ScoobiConfiguration2Reader[ScoobiConfiguration2]): ScoobiConfiguration2State[A] =
    ScoobiConfiguration2State(c => read(f(c)))

}

object ScoobiConfiguration2State {
  def apply[A](r: ScoobiConfiguration2 => (A, ScoobiConfiguration2)): ScoobiConfiguration2State[A] =
    new ScoobiConfiguration2State[A] {
      val read = r
    }

  def value[A](a: => A): ScoobiConfiguration2State[A] =
    apply(c => (a, c))

  def put(c: => ScoobiConfiguration2): ScoobiConfiguration2State[Unit] =
    apply(_ => ((), c))

  val get: ScoobiConfiguration2State[ScoobiConfiguration2] =
    apply(c => (c, c))

  def modify(f: ScoobiConfiguration2Reader[ScoobiConfiguration2]): ScoobiConfiguration2State[Unit] =
    apply(c => ((), f(c)))

}
