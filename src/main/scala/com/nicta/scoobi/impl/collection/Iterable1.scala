package com.nicta.scoobi
package impl
package collection

trait Iterable1[+A] {
  val head: A
  val tail: Iterable[A]

  import Iterator1._

  def iterator: Iterator1[A] =
    head +: tail.iterator

  // flatten, transpose, isEmpty, size, hasDefiniteSize, ++, map, flatMap, filter, remove, partition, groupBy, foreach, forall, exists, count, find, foldLeft, /:, reduceLeft, foldRight, :\, last, init, take, drop, slice, takeWhile, dropWhile, span, copyToArray, toArray, toList, toSeq, toStream, sortWith, mkString, toString, stringPrefix
}

object Iterable1 {
  case class RichIterable[+A](it: Iterable[A]) {
    def +:[AA >: A](h: AA): Iterable1[AA] =
      new Iterable1[AA] {
        val head = h
        val tail = it
      }
  }

  implicit def IterableToIterable1[A](it: Iterable[A]): RichIterable[A] =
    RichIterable(it)
}