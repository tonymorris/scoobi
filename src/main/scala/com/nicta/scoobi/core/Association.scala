/**
 * Copyright 2011,2012 National ICT Australia Limited
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.nicta.scoobi
package core

import scalaz._, Scalaz._

/**
 * An association of a key to 0 or many values.
 */
sealed trait Association[+K, +V] {
  /**
   * The key.
   */
  val key: K

  /**
   * Zero or many values.
   */
  val values: Iterable[V]

  /**
   * Map a function on the association. Synonym for `:->`.
   */
  def map[W](f: V => W): Association[K, W] =
    Association(key, values map f)

  /**
   * Map a function on the association. Synonym for `map`.
   */
  def :->[W](f: V => W): Association[K, W] =
    map(f)

  /**
   * Run an effect on each value.
   */
  def foreach[W](f: V => Unit) =
    values foreach f

  /**
   * Run a function on the key to a new key. Synonym for `<-:`.
   */
  def mapKey[L](f: K => L): Association[L, V] =
    Association(f(key), values)

  /**
   * Run a function on the key to a new key. Synonym for `mapKey`.
   */
  def <-:[L](f: K => L): Association[L, V] =
    mapKey(f)

  /**
   * Run a function on the values to new values.
   */
  def mapValues[W](f: Iterable[V] => Iterable[W]): Association[K, W] =
    Association(key, f(values))

  /**
   * Map two functions on the key and values (binary map).
   */
  def bimap[W, L](f: K => L, g: V => W): Association[L, W] =
    Association(f(key), values map g)

  /**
   * Zip the values of this association with the given association to produce an iterable of pairs. Alias for `***`.
   */
  def zip[KK >: K, W](w: Association[KK, W])(implicit S: Semigroup[KK]): Association[KK, (V, W)] =
    Association(S.append(key, w.key), values zip w.values)

  /**
   * Zip the values of this association with the given association to produce an iterable of pairs. Alias for `zip`.
   */
  def ***[KK >: K, W](w: Association[KK, W])(implicit S: Semigroup[KK]): Association[KK, (V, W)] =
    zip(w)

  /**
   * Zip the key of this association with the given association to produce an iterable of pairs.
   */
  def productKey[L, VV >: V](w: Association[L, VV]): Association[(K, L), VV] =
    Association((key, w.key), values ++ w.values)

  /**
   * Zip the given association with this association, appending on keys and values. Alias for `<*>:`.
   */
  def ap[KK >: K, W](f: Association[KK, V => W])(implicit S: Semigroup[KK]): Association[KK, W] =
    Association(S.append(f.key, key), for {
      v <- values
      w <- f.values
    } yield w(v))

  /**
   * Zip the given association with this association, appending on keys and values. Alias for `ap``.
   */
  def <*>:[KK >: K, W](f: Association[KK, V => W])(implicit S: Semigroup[KK]): Association[KK, W] =
    ap(f)

  /**
   * Return the first value in the associated values.
   */
  def firstValue: V =
    values.head

  /**
   * Traverse this association with the given function on an arbitrary functor.
   */
  def traverseKey[F[_]: Functor, L, VV >: V](f: K => F[L]): F[Association[L, VV]] =
    implicitly[Functor[F]].map(f(key))(l => Association(l, values))

  /**
   * True if all values of the association satisfy the given predicate.
   */
  def forall(p: V => Boolean): Boolean =
    values forall p

  /**
   * True if any values of the association satisfy the given predicate.
   */
  def exists(p: V => Boolean): Boolean =
    values exists p

  /**
   * Return the first element in the values satisfying the given predicate.
   */
  def find(p: V => Boolean): Option[V] =
    values find p

  /**
   * Return the number of values in the association.
   */
  def nvalues: Int =
    values.size

  override def toString: String =
    "Association(" + key + ", " + values.toString + ")"
}

object Association {
  /**
   * Construct an association with the given key and values.
   */
  def apply[K, V](k: K, vs: Iterable[V]): Association[K, V] =
    new Association[K, V] {
      val key =
        k
      val values =
        vs
    }

  /**
   * Construct an association with the given key and one value.
   */
  def single[K, V](k: K, v: V): Association[K, V] =
    apply(k, Iterable(v))

  /**
   * Construct an association with the given key and values.
   */
  def many[K, V](k: K, vs: V*): Association[K, V] =
    apply(k, Iterable(vs: _*))

  /**
   * A lens on the key of an association.
   */
  def keyL[K, V]: Association[K, V] @> K =
    Lens(a => Store(Association(_, a.values), a.key))

  /**
   * A lens on the values of an association.
   */
  def valuesL[K, V]: Association[K, V] @> Iterable[V] =
    Lens(a => Store(Association(a.key, _), a.values))

  implicit def AssociationFunctor[K]: Functor[({type λ[α] = Association[K, α]})#λ] =
    new Functor[({type λ[α] = Association[K, α]})#λ] {
      def map[A, B](a: Association[K, A])(f: A => B) =
        a map f
    }

  implicit def AssociationZip[K: Semigroup]: Zip[({type λ[α] = Association[K, α]})#λ] =
    new Zip[({type λ[α] = Association[K, α]})#λ] {
      def zip[A, B](a: => Association[K, A], b: => Association[K, B]) =
        a zip b
    }

  implicit def AssociationApplyZip[K: Semigroup]: Zip[({type λ[α] = Association[K, α]})#λ] with Apply[({type λ[α] = Association[K, α]})#λ] =
    new Zip[({type λ[α] = Association[K, α]})#λ] with Apply[({type λ[α] = Association[K, α]})#λ] {
      override def map[A, B](a: Association[K, A])(f: A => B) =
        a map f
      override def zip[A, B](a: => Association[K, A], b: => Association[K, B]) =
        a zip b
      def ap[A, B](a: => Association[K, A])(f: => Association[K, A => B]) =
        a ap f
    }

  implicit val AssociationBifunctor: Bifunctor[Association] =
    new Bifunctor[Association] {
      def bimap[A, B, C, D](a: Association[A, B])(f: A => C, g: B => D): Association[C, D] =
        a bimap (f, g)
    }

  implicit def AssociationWireFormat[K: WireFormat, V: WireFormat]: WireFormat[Association[K, V]] = {
    val k = implicitly[WireFormat[(K, Iterable[V])]]
    k xmap
      (e => Association(e._1, e._2), (q: Association[K, V]) => (q.key, q.values))
  }

}
