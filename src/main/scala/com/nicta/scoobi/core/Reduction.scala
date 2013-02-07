package com.nicta.scoobi
package core

import scalaz._, Scalaz._, BijectionT._

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

  def pairE: Endo[A] =
    Endo(pair)

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

  def option(r: Reduction[A]): Reduction[Option[A]] =
    Reduction((a1, a2) => a1 flatMap (aa1 => a2 map (r reduce (aa1, _))))

  def pointwise[B]: Reduction[B => A] =
    Reduction((f, g) => b => reduce(f(b), g(b)))

  def xmap[B](f: A => B, g: B => A): Reduction[B] =
    Reduction((b1, b2) => f(reduce(g(b1), g(b2))))

  def biject[B](b: Bijection[A, B]): Reduction[B] =
    xmap(b to _, b from _)

  def semigroup: Semigroup[A] =
    Semigroup.instance((a1, a2) => reduce(a1, a2))

  def apply: Apply[({type lam[a]=A})#lam] = new Apply[({type lam[a]=A})#lam] {
    override def map[X, Y](a: A)(f: X => Y) = a
    def ap[X, Y](a: => A)(f: => A) = reduce(f, a)
  }

  def compose: Compose[({type lam[a, b]=A})#lam] = new Compose[({type lam[a, b]=A})#lam] {
    def compose[X, Y, Z](f: A, g: A) = reduce(f, g)
  }

  class Associative {
    def law(a1: A, a2: A, a3: A)(implicit E: Equal[A]): Boolean =
      reduce(reduce(a1, a2), a3) === reduce(a1, (reduce(a2, a3)))
  }
  def associative: Associative = new Associative

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

  def split1E[A](e: Endo[A]): Reduction[A] =
    split1(e.run)

  def split2[A](f: A => A): Reduction[A] =
    apply((_, a2) => f(a2))

  def split2E[A](e: Endo[A]): Reduction[A] =
    split2(e.run)

  def unit: Reduction[Unit] =
    constant(())

  def firstOption[A]: Reduction[Option[A]] =
    Reduction((a1, a2) => a1 orElse a2)

  def lastOption[A]: Reduction[Option[A]] =
    Reduction((a1, a2) => a2 orElse a1)

  def minimum[A](implicit O: Order[A]): Reduction[A] =
    Reduction((a1, a2) => O min (a1, a2))

  def minimumOption[A](implicit O: Order[A]): Reduction[Option[A]] =
    Reduction((a1, a2) => Order[Option[A]] min (a1, a2))

  def maximumOption[A](implicit O: Order[A]): Reduction[Option[A]] =
    Reduction((a1, a2) => Order[Option[A]] max (a1, a2))

  def maximum[A](implicit O: Order[A]): Reduction[A] =
    Reduction((a1, a2) => O max (a1, a2))

  def endo[A]: Reduction[A => A] =
    Reduction((a1, a2) => a1 compose a2)

  def endoE[A]: Reduction[Endo[A]] =
    Reduction((a1, a2) => a1 compose a2)

  def or: Reduction[Boolean] =
    Reduction(_ || _)

  def and: Reduction[Boolean] =
    Reduction(_ && _)

  def ordering: Reduction[Ordering] =
    Reduction((a1, a2) => a1 match {
      case Ordering.EQ => a2
      case _ => a1
    })

  def comparator[A]: Reduction[java.util.Comparator[A]] =
    Reduction((c1, c2) => new java.util.Comparator[A] {
      def compare(a1: A, a2: A) =
        c1.compare(a1, a2) match {
          case 0 => c2.compare(a1, a2)
          case n => n
        }
    })

  def orderingS[A]: Reduction[math.Ordering[A]] =
    Reduction((o1, o2) => new math.Ordering[A] {
      def compare(a1: A, a2: A) =
        o1.compare(a1, a2) match {
          case 0 => o2.compare(a1, a2)
          case n => n
        }
    })

  def string: Reduction[String] =
    Reduction(_ + _)

  def list[A]: Reduction[List[A]] =
    Reduction(_ ::: _)

  def stream[A]: Reduction[Stream[A]] =
    Reduction(_ #::: _)

  def ephemeralStream[A]: Reduction[EphemeralStream[A]] =
    Reduction(_ ++ _)

  def vector[A]: Reduction[Vector[A]] =
    Reduction(_ ++ _)

  def array[A: Manifest]: Reduction[Array[A]] =
    Reduction((c1, c2) => (c1 ++ c2).toArray)

  def differenceList[A]: Reduction[DList[A]] =
    Reduction(_ ++ _)

  def intmap[A]: Reduction[collection.immutable.IntMap[A]] =
    Reduction(_ ++ _)

  def longmap[A]: Reduction[collection.immutable.LongMap[A]] =
    Reduction(_ ++ _)

  def hashmap[A, B]: Reduction[collection.immutable.HashMap[A, B]] =
    Reduction(_ ++ _)

  def hashset[A]: Reduction[collection.immutable.HashSet[A]] =
    Reduction(_ ++ _)

  def treemap[A, B]: Reduction[collection.immutable.TreeMap[A, B]] =
    Reduction(_ ++ _)

  def treeset[A]: Reduction[collection.immutable.TreeSet[A]] =
    Reduction(_ ++ _)

  def listmap[A, B]: Reduction[collection.immutable.ListMap[A, B]] =
    Reduction(_ ++ _)

  def listset[A]: Reduction[collection.immutable.ListSet[A]] =
    Reduction(_ ++ _)

  def queue[A]: Reduction[collection.immutable.Queue[A]] =
    Reduction(_ ++ _)

  def stack[A]: Reduction[collection.immutable.Stack[A]] =
    Reduction(_ ++ _)

  object Sum {
    def bigdecimal: Reduction[BigDecimal] =
      Reduction(_ + _)

    def jbigdecimal: Reduction[java.math.BigDecimal] =
      Reduction(_ add _)

    def bigint: Reduction[BigInt] =
      Reduction(_ + _)

    def biginteger: Reduction[java.math.BigInteger] =
      Reduction(_ add _)

    def byte: Reduction[Byte] =
      Reduction((c1, c2) => (c1 + c2).toByte)

    def char: Reduction[Char] =
      Reduction((c1, c2) => (c1 + c2).toChar)

    def double: Reduction[Double] =
      Reduction(_ + _)

    def float: Reduction[Float] =
      Reduction(_ + _)

    def int: Reduction[Int] =
      Reduction(_ + _)

    def long: Reduction[Int] =
      Reduction(_ + _)

    def short: Reduction[Short] =
      Reduction((c1, c2) => (c1 + c2).toShort)
  }

  object Product {
    def bigdecimal: Reduction[BigDecimal] =
      Reduction(_ * _)

    def jbigdecimal: Reduction[java.math.BigDecimal] =
      Reduction(_ multiply _)

    def bigint: Reduction[BigInt] =
      Reduction(_ * _)

    def biginteger: Reduction[java.math.BigInteger] =
      Reduction(_ multiply _)

    def byte: Reduction[Byte] =
      Reduction((c1, c2) => (c1 * c2).toByte)

    def char: Reduction[Char] =
      Reduction((c1, c2) => (c1 * c2).toChar)

    def double: Reduction[Double] =
      Reduction(_ * _)

    def float: Reduction[Float] =
      Reduction(_ * _)

    def int: Reduction[Int] =
      Reduction(_ * _)

    def long: Reduction[Int] =
      Reduction(_ * _)

    def short: Reduction[Short] =
      Reduction((c1, c2) => (c1 * c2).toShort)
  }
}
