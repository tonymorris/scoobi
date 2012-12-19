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
package impl
package collection

/**
 * A non-empty iterator contains at least one element. Consequences include:
 *
 * * `reduceLeft` will always produce a value.
 * * `first` will always produce a value.
 * * `next` will always produce a value on its first invocation.
 * * `hasNext` will always return true on its first invocation.
 *
 * Some operations on a non-empty iterable result in a non-empty iterable.
 *
 * '''NOTE: Most Iterator functions perform SIDE-EFFECTS and so EQUATIONAL REASONING DOES NOT APPLY.'''
 */
trait Iterator1[+A] extends TraversableOnce[A] {
  /**
   * The constant first element of this iterator.
   */
  def first: A
  private[collection] val rest: Iterator[A]

  import Iterator1._

  private var fnext: Boolean = false

  /**
   * True if this iterator can produce a value from `next`.
   */
  def hasNext: Boolean =
    !fnext || rest.hasNext

  /**
   * Produces a value if possible, or throws an exception if not possible. The method `hasNext` determines if a value can be produced.
   */
  def next: A =
    if(fnext)
      rest.next
    else {
      fnext = true
      first
    }

  /**
   * Return a regular iterator, losing the non-empty invariant in the type.
   */
  def seq: Iterator[A] =
    toIterator

  /**
   * Returns a stream of this iterator.
   */
  def toTraversable: Traversable[A] =
    toStream

  /**
   * Copies this iterator to an array at the given interval.
   */
  def copyToArray[AA >: A](xs: Array[AA], start: Int, n: Int): Unit = {
    var i = start
    val end = math.min(start + n, xs.length)
    while (hasNext && i < end) {
      xs(i) = next
      i += 1
    }
  }

  /**
   * A non-empty iterator always has a definite size.
   */
  def hasDefiniteSize: Boolean =
    true

  /**
   * A non-empty iterator is never traversable again.
   */
  def isTraversableAgain: Boolean =
    false

  /**
   * Return a regular iterator, losing the non-empty invariant in the type.
   */
  def toIterator: Iterator[A] =
    new Iterator[A] {
      def hasNext =
        Iterator1.this.hasNext

      def next =
        Iterator1.this.next
    }

  /**
   * The negation of `hasNext`.
   */
  def isEmpty =
    !hasNext

  /**
   * Take at most the given number of elements from the front of the iterable.
   */
  def take(n: Int): Iterator[A] =
    toIterator take n

  def drop(n: Int): Iterator[A] =
    toIterator drop n

  def slice(from: Int, to: Int): Iterator[A] =
    toIterator slice (from, to)

  def map[B](f: A => B): Iterator1[B] =
    f(first) +:: (rest map f)

  def ++[AA >: A](that: => Iterator1[AA]): Iterator1[AA] =
    first +:: (rest ++ that.toIterator)

  def flatMap[B](f: A => Iterator1[B]): Iterator1[B] = {
    val k = f(first)
    k.first +:: (k.rest ++ (rest flatMap (f(_).toIterator)))
  }

  def filter(p: A => Boolean): Iterator[A] =
    toIterator filter p

  def withFilter(p: A => Boolean): Iterator[A] =
    toIterator withFilter p

  def filterNot(p: A => Boolean): Iterator[A] =
    toIterator filterNot p

  def collect[B](pf: PartialFunction[A, B]): Iterator[B] =
    toIterator collect pf

  def takeWhile(p: A => Boolean): Iterator[A] =
    toIterator takeWhile p

  def partition(p: A => Boolean): BreakIterator1[A] = {
    val (xx, yy) = rest partition p
    if(p(first))
      LeftBreakIterator1(first +:: xx, yy)
    else
      RightBreakIterator1(xx, first +:: yy)
  }

  def span(p: A => Boolean): BreakIterator1[A] = {
    if(p(first)) {
      val (xx, yy) = rest span p
      LeftBreakIterator1(first +:: xx, yy)
    } else
      RightBreakIterator1(Iterator.empty, first +:: rest)
  }

  def dropWhile(p: A => Boolean): Iterator[A] =
    toIterator dropWhile p

  def zip[B](that: Iterator1[B]): Iterator1[(A, B)] =
    (first, that.first) +:: (rest zip that.rest)

  def padTo[AA >: A](len: Int, elem: AA): Iterator1[AA] =
    first +:: (rest padTo (len - 1, elem))

  def zipWithIndex: Iterator1[(A, Int)] =
    (first, 0) +:: (rest.zipWithIndex map {
      case (a, n) => (a, n + 1)
    })

  def zipAll[B, AA >: A, BB >: B](that: Iterator1[B], thisElem: AA, thatElem: BB): Iterator1[(AA, BB)] =
    (first, that.first) +:: (rest zipAll (that.rest, thisElem, thatElem))

  def foreach[U](f: A => U) = {
    f(first)
    rest foreach f
  }

  def forall(p: A => Boolean): Boolean =
    p(first) && (rest forall p)

  def exists(p: A => Boolean): Boolean =
    p(first) || (rest exists p)

  def contains(elem: Any): Boolean =
    first == elem || (rest contains elem)

  def find(p: A => Boolean): Option[A] =
    if(p(first))
      Some(first)
    else
      rest find p

  def indexWhere(p: A => Boolean): Int =
    if(p(first))
      0
    else {
      val i = rest indexWhere p
      if(i == -1)
        -1
      else
        i + 1
    }

  def indexOf[AA >: A](elem: AA): Int =
    if(first == elem)
      0
    else {
      val i = rest indexOf elem
      if(i == -1)
        -1
      else
        i + 1
    }

  def length: Int =
    1 + rest.length

  def duplicate: (Iterator1[A], Iterator1[A]) = {
    val (x, y) = rest.duplicate
    (first +:: x, first +:: y)
  }

  def sameElements(that: Iterator1[_]): Boolean =
    (first == that.first) && (rest sameElements that.rest)

  def toStream: Stream[A] =
    first #:: rest.toStream

  override def toString =
    "non-empty iterator (Iterator1)"

}

object Iterator1 {
  // CAUTION
  private def unsafeIterator1[A](it: Iterator[A]): Iterator1[A] =
    if(it.hasNext) {
      val h = it.next
      h +:: it
    } else
      sys.error("Invariant broken. Iterator1#unsafeIterator1 was invoked on an empty Iterator.")

  case class RichIterator[+A](it: Iterator[A]) {
    def +::[AA >: A](h: AA): Iterator1[AA] =
      new Iterator1[AA] {
        def first = h
        val rest = it
      }

    def scan1Left[B](z: B)(op: (B, A) => B): Iterator1[B] =
      unsafeIterator1(it.scanLeft(z)(op))

    def scan1Right[B](z: B)(op: (A, B) => B): Iterator1[B] =
      unsafeIterator1(it.scanRight(z)(op))

  }

  implicit def IteratorToIterator1[A](it: Iterator[A]): RichIterator[A] =
    RichIterator(it)

  sealed trait BreakIterator1[+A]
  case class LeftBreakIterator1[+A](x: Iterator1[A], y: Iterator[A]) extends BreakIterator1[A]
  case class RightBreakIterator1[+A](x: Iterator[A], y: Iterator1[A]) extends BreakIterator1[A]

  def single[A](elem: A): Iterator1[A] =
    elem +:: Iterator.empty

  def apply[A](elem: A, elems: A*): Iterator1[A] =
    elem +:: Iterator(elems: _*)

  def iterate[A](start: A)(f: A => A): Iterator1[A] =
    start +:: Iterator.iterate(f(start))(f)

  def from(start: Int): Iterator1[Int] =
    start +:: Iterator.from(start + 1)

  def from(start: Int, step: Int): Iterator1[Int] =
    start +:: Iterator.from(start + step)

}
