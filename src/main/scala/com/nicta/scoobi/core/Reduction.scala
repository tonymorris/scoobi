package com.nicta.scoobi
package core

trait Reduction[A] {
  val reduce: (A, A) => A

  def zip[B](r: Reduction[B]): Reduction[(A, B)] =
    Reduction {
      case ((a1, b1), (a2, b2)) => (reduce(a1, a2), r reduce (b1, b2))
    }

  def ***[B](r: Reduction[B]): Reduction[(A, B)] =
    zip(r)

}

object Reduction {
  def apply[A](f: (A, A) => A): Reduction[A] =
    new Reduction[A] {
      val reduce = f
    }

  def constant[A](a: => A): Reduction[A] =
    apply((_, _) => a)
}
