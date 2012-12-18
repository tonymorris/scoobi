package com.nicta.scoobi
package impl
package collection

trait Iterator1[+A] extends TraversableOnce[A] {
  val head: A
  val tail: Iterator[A]

  import Iterator1._

  private var fnext: Boolean = false

  def hasNext: Boolean =
    !fnext || tail.hasNext

  def next: A =
    if(fnext)
      tail.next
    else {
      fnext = true
      head
    }

  def seq: Iterator[A] =
    toIterator

  def toTraversable: Traversable[A] =
    toStream

  def copyToArray[AA >: A](xs: Array[AA], start: Int, n: Int): Unit = {
    var i = start
    val end = math.min(start + n, xs.length)
    while (hasNext && i < end) {
      xs(i) = next
      i += 1
    }
  }

  def hasDefiniteSize: Boolean =
    true

  def isTraversableAgain: Boolean =
    false

  def toIterator: Iterator[A] =
    new Iterator[A] {
      def hasNext =
        Iterator1.this.hasNext

      def next =
        Iterator1.this.next
    }

  def isEmpty =
    !hasNext

  def take(n: Int): Iterator[A] =
    toIterator take n

  def drop(n: Int): Iterator[A] =
    toIterator drop n

  def slice(from: Int, to: Int): Iterator[A] =
    toIterator slice (from, to)

  def map[B](f: A => B): Iterator1[B] =
    f(head) +: (tail map f)

  def ++[AA >: A](that: => Iterator1[AA]): Iterator1[AA] =
    head +: (tail ++ that.toIterator)

  def flatMap[B](f: A => Iterator1[B]): Iterator1[B] = {
    val k = f(head)
    k.head +: (k.tail ++ (tail flatMap (f(_).toIterator)))
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
    val (xx, yy) = tail partition p
    if(p(head))
      LeftBreakIterator1(head +: xx, yy)
    else
      RightBreakIterator1(xx, head +: yy)
  }

  def span(p: A => Boolean): BreakIterator1[A] = {
    if(p(head)) {
      val (xx, yy) = tail span p
      LeftBreakIterator1(head +: xx, yy)
    } else
      RightBreakIterator1(Iterator.empty, head +: tail)
  }

  def dropWhile(p: A => Boolean): Iterator[A] =
    toIterator dropWhile p

  def zip[B](that: Iterator1[B]): Iterator1[(A, B)] =
    (head, that.head) +: (tail zip that.tail)

  def padTo[AA >: A](len: Int, elem: AA): Iterator1[AA] =
    head +: (tail padTo (len - 1, elem))

  def zipWithIndex: Iterator1[(A, Int)] =
    (head, 0) +: (tail.zipWithIndex map {
      case (a, n) => (a, n + 1)
    })

  def zipAll[B, AA >: A, BB >: B](that: Iterator1[B], thisElem: AA, thatElem: BB): Iterator1[(AA, BB)] =
    (head, that.head) +: (tail zipAll (that.tail, thisElem, thatElem))

  def foreach[U](f: A => U) = {
    f(head)
    tail foreach f
  }

  def forall(p: A => Boolean): Boolean =
    p(head) && (tail forall p)

  def exists(p: A => Boolean): Boolean =
    p(head) || (tail exists p)

  def contains(elem: Any): Boolean =
    head == elem || (tail contains elem)

  def find(p: A => Boolean): Option[A] =
    if(p(head))
      Some(head)
    else
      tail find p

  def indexWhere(p: A => Boolean): Int =
    if(p(head))
      0
    else {
      val i = tail indexWhere p
      if(i == -1)
        -1
      else
        i + 1
    }

  def indexOf[AA >: A](elem: AA): Int =
    if(head == elem)
      0
    else {
      val i = tail indexOf elem
      if(i == -1)
        -1
      else
        i + 1
    }

  def length: Int =
    1 + tail.length

  def duplicate: (Iterator1[A], Iterator1[A]) = {
    val (x, y) = tail.duplicate
    (head +: x, head +: y)
  }

  def sameElements(that: Iterator1[_]): Boolean =
    (head == that.head) && (tail sameElements that.tail)

  def toStream: Stream[A] =
    head #:: tail.toStream

  override def toString =
    "non-empty iterator (Iterator1)"

}

object Iterator1 {
  // CAUTION
  private def unsafeIterator1[A](it: Iterator[A]): Iterator1[A] =
    if(it.hasNext) {
      val h = it.next
      h +: it
    } else
      sys.error("Invariant broken. Iterator1#unsafeIterator1 was invoked on an empty Iterator.")

  case class RichIterator[+A](it: Iterator[A]) {
    def +:[AA >: A](h: AA): Iterator1[AA] =
      new Iterator1[AA] {
        val head = h
        val tail = it
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
    elem +: Iterator.empty

  def apply[A](elem: A, elems: A*): Iterator1[A] =
    elem +: Iterator(elems: _*)

  def iterate[A](start: A)(f: A => A): Iterator1[A] =
    start +: Iterator.iterate(f(start))(f)

  def from(start: Int): Iterator1[Int] =
    start +: Iterator.from(start + 1)

  def from(start: Int, step: Int): Iterator1[Int] =
    start +: Iterator.from(start + step)

}
