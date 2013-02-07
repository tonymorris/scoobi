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

  def zip3[B, C](b: Reduction[B], c: Reduction[C]): Reduction[(A, B, C)] =
    Reduction {
      case ((a1, b1, c1), (a2, b2, c2)) => (reduce(a1, a2), b reduce (b1, b2), c reduce (c1, c2))
    }

  def zip4[B, C, D](b: Reduction[B], c: Reduction[C], d: Reduction[D]): Reduction[(A, B, C, D)] =
    Reduction {
      case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => (reduce(a1, a2), b reduce (b1, b2), c reduce (c1, c2), d reduce (d1, d2))
    }

  def zip5[B, C, D, E](b: Reduction[B], c: Reduction[C], d: Reduction[D], e: Reduction[E]): Reduction[(A, B, C, D, E)] =
    Reduction {
      case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) => (reduce(a1, a2), b reduce (b1, b2), c reduce (c1, c2), d reduce (d1, d2), e reduce (e1, e2))
    }

  def wzip[W](implicit S: Semigroup[W]): Reduction[(W, A)] =
    Reduction {
      case ((w1, a1), (w2, a2)) => (S.append(w1, w2), reduce(a1, a2))
    }

  def wzip2[W, X](implicit SW: Semigroup[W], SX: Semigroup[X]): Reduction[(W, X, A)] =
    Reduction {
      case ((w1, x1, a1), (w2, x2, a2)) => (SW.append(w1, w2), SX.append(x1, x2), reduce(a1, a2))
    }

  def wzip3[W, X, Y](implicit SW: Semigroup[W], SX: Semigroup[X], SY: Semigroup[Y]): Reduction[(W, X, Y, A)] =
    Reduction {
      case ((w1, x1, y1, a1), (w2, x2, y2, a2)) => (SW.append(w1, w2), SX.append(x1, x2), SY.append(y1, y2), reduce(a1, a2))
    }

  def wzip4[W, X, Y, Z](implicit SW: Semigroup[W], SX: Semigroup[X], SY: Semigroup[Y], SZ: Semigroup[Z]): Reduction[(W, X, Y, Z, A)] =
    Reduction {
      case ((w1, x1, y1, z1, a1), (w2, x2, y2, z2, a2)) => (SW.append(w1, w2), SX.append(x1, x2), SY.append(y1, y2), SZ.append(z1, z2), reduce(a1, a2))
    }

  def wzip5[W, X, Y, Z, V](implicit SW: Semigroup[W], SX: Semigroup[X], SY: Semigroup[Y], SZ: Semigroup[Z], SV: Semigroup[V]): Reduction[(W, X, Y, Z, V, A)] =
    Reduction {
      case ((w1, x1, y1, z1, v1, a1), (w2, x2, y2, z2, v2, a2)) => (SW.append(w1, w2), SX.append(x1, x2), SY.append(y1, y2), SZ.append(z1, z2), SV.append(v1, v2), reduce(a1, a2))
    }

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
        case \/-(b2) => \/-(b2)
      }
      case \/-(b1) => x2 match {
        case -\/(a2) => -\/(a2)
        case \/-(b2) => \/-(r reduce (b1, b2))
      }
    })

  def option(r: Reduction[A]): Reduction[Option[A]] =
    Reduction((a1, a2) => a1 flatMap (aa1 => a2 map (r reduce (aa1, _))))

  def pointwise[B]: Reduction[B => A] =
    Reduction((g, h) => b => reduce(g(b), h(b)))

  def pointwise2[B, C]: Reduction[(B, C) => A] =
    Reduction((g, h) => (b, c) => reduce(g(b, c), h(b, c)))

  def pointwise3[B, C, D]: Reduction[(B, C, D) => A] =
    Reduction((g, h) => (b, c, d) => reduce(g(b, c, d), h(b, c, d)))

  def pointwise4[B, C, D, E]: Reduction[(B, C, D, E) => A] =
    Reduction((g, h) => (b, c, d, e) => reduce(g(b, c, d, e), h(b, c, d, e)))

  def pointwise5[B, C, D, E, F]: Reduction[(B, C, D, E, F) => A] =
    Reduction((g, h) => (b, c, d, e, f) => reduce(g(b, c, d, e, f), h(b, c, d, e, f)))

  def pointwiseK[Q[+_], B](implicit A: Apply[Q]): Reduction[Kleisli[Q, B, A]] =
    Reduction((g, h) => Kleisli(
      b => A.apply2(g(b), h(b))(reduce(_, _))
    ))

  def pointwise2K[Q[+_], B, C](implicit A: Apply[Q]): Reduction[Kleisli[Q, (B, C), A]] =
    Reduction((g, h) => Kleisli {
      case (b, c) => A.apply2(g(b, c), h(b, c))(reduce(_, _))
    })

  def pointwise3K[Q[+_], B, C, D](implicit A: Apply[Q]): Reduction[Kleisli[Q, (B, C, D), A]] =
    Reduction((g, h) => Kleisli {
      case (b, c, d) => A.apply2(g(b, c, d), h(b, c, d))(reduce(_, _))
    })

  def pointwise4K[Q[+_], B, C, D, E](implicit A: Apply[Q]): Reduction[Kleisli[Q, (B, C, D, E), A]] =
    Reduction((g, h) => Kleisli {
      case (b, c, d, e) => A.apply2(g(b, c, d, e), h(b, c, d, e))(reduce(_, _))
    })

  def pointwise5K[Q[+_], B, C, D, E, F](implicit A: Apply[Q]): Reduction[Kleisli[Q, (B, C, D, E, F), A]] =
    Reduction((g, h) => Kleisli {
      case (b, c, d, e, f) => A.apply2(g(b, c, d, e, f), h(b, c, d, e, f))(reduce(_, _))
    })

  def validation[B](b: Reduction[B]): Reduction[Validation[A, B]] =
    Reduction((v1, v2) => v1 match {
      case Failure(a1) => v2 match {
        case Failure(a2) => Failure(reduce (a1, a2))
        case Success(b2) => Success(b2)
      }
      case Success(b1) => v2 match {
        case Failure(a2) => Failure(a2)
        case Success(b2) => Success(b reduce (b1, b2))
      }
    })

  def store[B](r: Reduction[B]): Reduction[Store[A, B]] =
    Reduction((s1, s2) =>
      Store(r.pointwise[A] reduce (s1 put _, s2 put _), reduce(s1.pos, s2.pos))
    )

  def state[S]: Reduction[State[S, A]] =
    Reduction((s1, s2) =>
      s1 flatMap (a1 => s2 map (a2 => reduce(a1, a2))))

  def writer[W: Semigroup]: Reduction[Writer[W, A]] =
    Reduction((w1, w2) =>
      w1 flatMap (a1 => w2 map (a2 => reduce(a1, a2))))

  def xmap[B](f: A => B, g: B => A): Reduction[B] =
    Reduction((b1, b2) => f(reduce(g(b1), g(b2))))

  def biject[B](b: Bijection[A, B]): Reduction[B] =
    xmap(b to _, b from _)

  def on(f: A => A): Reduction[A] =
    xmap(f, f)

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

  def xor: Reduction[Boolean] =
    Reduction(_ != _)

  def biimplication: Reduction[Boolean] =
    Reduction(_ == _)

  def equal[A](implicit E: Equal[A]): Reduction[Equal[A]] =
    Reduction((e1, e2) => new Equal[A] {
      def equal(a1: A, a2: A) =
        E equal (a1, a2)
    })

  def order[A](implicit O: Order[A]): Reduction[Order[A]] =
    Reduction((e1, e2) => new Order[A] {
      def order(a1: A, a2: A) =
        O order (a1, a2)
    })

  def show[A](implicit S: Show[A]): Reduction[Show[A]] =
    Reduction((e1, e2) => new Show[A] {
      override def show(a: A) =
        S show a
    })

  def ordering: Reduction[Ordering] =
    Reduction((a1, a2) => a1 match {
      case Ordering.EQ => a2
      case _ => a1
    })

  def comparable[A]: Reduction[Comparable[A]] =
    Reduction((c1, c2) => new Comparable[A] {
      def compareTo(a: A) =
        c1.compareTo(a) match {
          case 0 => c2.compareTo(a)
          case n => n
        }
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

  def nonEmptyList[A]: Reduction[NonEmptyList[A]] =
    Reduction(_ append _)

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

  def nodeseq: Reduction[xml.NodeSeq] =
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

    def digit: Reduction[Digit] =
      Reduction((d1, d2) => Digit.mod10Digit(d1.toInt + d2.toInt))

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

    def digit: Reduction[Digit] =
      Reduction((d1, d2) => Digit.mod10Digit(d1.toInt * d2.toInt))

  }
}
