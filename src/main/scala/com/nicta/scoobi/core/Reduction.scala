package com.nicta.scoobi
package core

import scalaz._

trait Reduction[A] {
  val reduce: (A, A) => A

  def reduceC: A => A => A =
    reduce.curried

  def dual: Reduction[A] =
    Reduction((a1, a2) => reduce(a2, a1))

  def unary_~ : Reduction[A] =
    dual

  def pair: A => A =
    a => reduce(a, a)

  def zip[B](r: Reduction[B]): Reduction[(A, B)] =
    Reduction {
      case ((a1, b1), (a2, b2)) => (reduce(a1, a2), r reduce (b1, b2))
    }

  def ***[B](r: Reduction[B]): Reduction[(A, B)] =
    zip(r)

  def left[B](r: => Reduction[B]): Reduction[A \/ B] =
    Reduction((x1, x2) => x1 match {
      case -\/(a1) => x2 match {
        case -\/(a2) => -\/(reduce(a1, a2))
        case \/-(_) => -\/(a1)
      }
      case \/-(b1) => x2 match {
        case -\/(_) => \/-(b1)
        case \/-(b2) => \/-(r reduce (b1, b2))
      }
    })

  def right[B](r: => Reduction[B]): Reduction[A \/ B] =
    Reduction((x1, x2) => x1 match {
      case -\/(a1) => x2 match {
        case -\/(a2) => -\/(reduce(a1, a2))
        case \/-(b1) => \/-(b1)
      }
      case \/-(b1) => x2 match {
        case -\/(a1) => -\/(a1)
        case \/-(b2) => \/-(r reduce (b1, b2))
      }
    })

  def xmap[B](f: A => B, g: B => A): Reduction[B] =
    Reduction((b1, b2) => f(reduce(g(b1), g(b2))))

  def semigroup: Semigroup[A] =
    Semigroup.instance((a1, a2) => reduce(a1, a2))
}

object Reduction {
  def apply[A](f: (A, A) => A): Reduction[A] =
    new Reduction[A] {
      val reduce = f
    }

  def constant[A](a: => A): Reduction[A] =
    apply((_, _) => a)

  def split1[A](f: A => A): Reduction[A] =
    apply((a1, _) => f(a1))

  def split2[A](f: A => A): Reduction[A] =
    apply((_, a2) => f(a2))

}
