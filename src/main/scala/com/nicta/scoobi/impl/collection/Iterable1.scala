package com.nicta.scoobi
package impl
package collection

trait Iterable1[+A] {
  val head: A
  val tail: Iterable[A]

  import Iterator1._
  import Iterable1._

  def iterator: Iterator1[A] =
    head +:: tail.iterator

  def toIterable: Iterable[A] =
    new Iterable[A] {
      def iterator =
        Iterable1.this.iterator.toIterator
    }

  def flatten[I](implicit I1: A => Iterable1[I]): Iterable1[I] = {
    val r = I1(head)
    r.head +:: (r.tail ++ tail.flatten(I1(_).iterator))
  }

  def size: Int =
    1 + tail.size

  def ++[AA >: A](that: => Iterable1[AA]): Iterable1[AA] =
    head +:: (tail ++ that.toIterable)

  def map[B](f: A => B): Iterable1[B] =
    f(head) +:: (tail map f)

  def flatMap[B](f: A => Iterable1[B]): Iterable1[B] = {
    val k = f(head)
    k.head +:: (k.tail ++ (tail flatMap (f(_).toIterable)))
  }

  def filter(p: A => Boolean): Iterable[A] =
    toIterable filter p

  def partition(p: A => Boolean): BreakIterable1[A] = {
    val (xx, yy) = tail partition p
    if(p(head))
      LeftBreakIterable1(head +:: xx, yy)
    else
      RightBreakIterable1(xx, head +:: yy)
  }

  def foreach[U](f: A => U) = {
    f(head)
    tail foreach f
  }

  def forall(p: A => Boolean): Boolean =
    p(head) && (tail forall p)

  def exists(p: A => Boolean): Boolean =
    p(head) || (tail exists p)

  def count(p: A => Boolean): Int = {
    var c = 0
    foreach(a => if(p(a)) c += 1)
    c
  }

  def find(p: A => Boolean): Option[A] =
    if(p(head))
      Some(head)
    else
      tail find p

  def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = {
    for (x <- iterator) {
      if (pf isDefinedAt x)
        return Some(pf(x))
    }
    None
  }

  def foldLeft[B](b: B)(op: (B, A) => B): B =
    toIterable.foldLeft(b)(op)

  def /:[B](b: B)(op: (B, A) => B): B =
    foldLeft(b)(op)

  def reduceLeft[AA >: A](op: (AA, A) => AA): AA =
    tail.foldLeft[AA](head)(op)

  def take(n: Int): Iterable[A] =
    toIterable take n

  def drop(n: Int): Iterable[A] =
    toIterable drop n

  def slice(from: Int, to: Int): Iterable[A] =
    toIterable slice (from, to)

  def takeWhile(p: A => Boolean): Iterable[A] =
    toIterable takeWhile p

  def dropWhile(p: A => Boolean): Iterable[A] =
    toIterable dropWhile p

  def span(p: A => Boolean): BreakIterable1[A] = {
    if(p(head)) {
      val (xx, yy) = tail span p
      LeftBreakIterable1(head +:: xx, yy)
    } else
      RightBreakIterable1(Iterable.empty, head +:: tail)
  }

  def toList: List[A] =
    toIterable.toList

  def toSeq: Seq[A] =
    toIterable.toSeq

  def toStream: Stream[A] =
    toIterable.toStream
}

object Iterable1 {
  case class RichIterable[+A](it: Iterable[A]) {
    def +::[AA >: A](h: AA): Iterable1[AA] =
      new Iterable1[AA] {
        val head = h
        val tail = it
      }
  }

  implicit def IterableToIterable1[A](it: Iterable[A]): RichIterable[A] =
    RichIterable(it)

  sealed trait BreakIterable1[+A]
  case class LeftBreakIterable1[+A](x: Iterable1[A], y: Iterable[A]) extends BreakIterable1[A]
  case class RightBreakIterable1[+A](x: Iterable[A], y: Iterable1[A]) extends BreakIterable1[A]

}
