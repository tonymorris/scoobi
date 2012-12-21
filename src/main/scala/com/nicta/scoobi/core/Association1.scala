package com.nicta.scoobi
package core

import impl.collection.{Iterator1, Iterable1}
import scalaz._, Scalaz._

sealed trait Association1[+K, +V] {
  val key: K
  val values: Iterable1[V]

  def map[W](f: V => W): Association1[K, W] =
    Association1(key, values map f)

  def :->[W](f: V => W): Association1[K, W] =
    map(f)

  def foreach[W](f: V => Unit) =
    values foreach f

  def mapKey[L](f: K => L): Association1[L, V] =
    Association1(f(key), values)

  def mapValues[W](f: Iterable1[V] => Iterable1[W]): Association1[K, W] =
    Association1(key, f(values))

  def <-:[L](f: K => L): Association1[L, V] =
    mapKey(f)

  def bimap[W, L](f: K => L, g: V => W): Association1[L, W] =
    Association1(f(key), values map g)

  def product[KK >: K, W](w: Association1[KK, W])(implicit S: Semigroup[KK]): Association1[KK, (V, W)] =
    Association1(S.append(key, w.key), values zip w.values)

  def ***[KK >: K, W](w: Association1[KK, W])(implicit S: Semigroup[KK]): Association1[KK, (V, W)] =
    product(w)

  def productKey[L, VV >: V](w: Association1[L, VV]): Association1[(K, L), VV] =
    Association1((key, w.key), values ++ w.values)

  def ap[KK >: K, W](f: Association1[KK, V => W])(implicit S: Semigroup[KK]): Association1[KK, W] =
    Association1(S.append(key, f.key), values ap f.values)

  def <*>:[KK >: K, W](f: Association1[KK, V => W])(implicit S: Semigroup[KK]): Association1[KK, W] =
    ap(f)

  def firstValue: V =
    values.head

  def traverseKey[F[_]: Applicative, L, VV >: V](f: K => F[L]): F[Association1[L, VV]] =
    implicitly[Functor[F]].map(f(key))(l => Association1(l, values))

  def forall(p: V => Boolean): Boolean =
    values forall p

  def exists(p: V => Boolean): Boolean =
    values exists p

  def find(p: V => Boolean): Option[V] =
    values find p

  def iterator: Iterator1[V] =
    values.iterator

  def nvalues: Int =
    values.size

  override def toString: String =
    "Association(" + key + ", " + values.toString + ")"
}

object Association1 {
  def apply[K, V](k: K, vs: Iterable1[V]): Association1[K, V] =
    new Association1[K, V] {
      val key =
        k
      val values =
        vs
    }

  def single[K, V](k: K, v: V): Association1[K, V] =
    apply(k, Iterable1.single(v))

  def many[K, V](k: K, v: V, vs: V*): Association1[K, V] =
    apply(k, Iterable1(v, vs: _*))

  def keyL[K, V]: Association1[K, V] @> K =
    Lens(a => Store(Association1(_, a.values), a.key))

  def valuesL[K, V]: Association1[K, V] @> Iterable1[V] =
    Lens(a => Store(Association1(a.key, _), a.values))

  def firstValueL[K, V]: Association1[K, V] @> V =
    valuesL >=> Iterable1.headL

  implicit def Association1Functor[K]: Functor[({type λ[α] = Association1[K, α]})#λ] =
    new Functor[({type λ[α] = Association1[K, α]})#λ] {
      def map[A, B](a: Association1[K, A])(f: A => B) =
        a map f
    }

  implicit def Association1Zip[K: Semigroup]: Zip[({type λ[α] = Association1[K, α]})#λ] =
    new Zip[({type λ[α] = Association1[K, α]})#λ] {
      def zip[A, B](a: => Association1[K, A], b: => Association1[K, B]) =
        a product b
    }

  implicit def Association1ApplyZip[K: Semigroup]: Zip[({type λ[α] = Association1[K, α]})#λ] with Apply[({type λ[α] = Association1[K, α]})#λ] =
    new Zip[({type λ[α] = Association1[K, α]})#λ] with Apply[({type λ[α] = Association1[K, α]})#λ] {
      override def map[A, B](a: Association1[K, A])(f: A => B) =
        a map f
      override def zip[A, B](a: => Association1[K, A], b: => Association1[K, B]) =
        a product b
      def ap[A, B](a: => Association1[K, A])(f: => Association1[K, A => B]) =
        a ap f
    }

  implicit val Associative1Bifunctor: Bifunctor[Association1] =
    new Bifunctor[Association1] {
      def bimap[A, B, C, D](a: Association1[A, B])(f: A => C, g: B => D): Association1[C, D] =
        a bimap (f, g)
    }

  implicit def Association1WireFormat[K: WireFormat, V: WireFormat]: WireFormat[Association1[K, V]] = {
    val k = implicitly[WireFormat[(K, Iterable1[V])]]
    k xmap
      (e => Association1(e._1, e._2), (q: Association1[K, V]) => (q.key, q.values))
  }
}
