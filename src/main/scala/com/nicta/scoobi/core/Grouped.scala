package com.nicta.scoobi
package core

import impl.collection.Iterable1

sealed trait Grouped[K, V] {
  val list: DList[(K, Iterable1[V])]

  //   def map[B: Manifest : WireFormat](f: A => B): DList[B] =

  def map[W](f: V => W)(implicit MK: Manifest[K], FK: WireFormat[K], MW: Manifest[W], FW: WireFormat[W]): Grouped[K, W] = {
    val g = implicitly[Manifest[(K, Iterable1[W])]]
    val h = implicitly[WireFormat[Iterable[W]]]
    // val h = implicitly[WireFormat[(K, Iterable1[W])]]
    Grouped(list.map(error(""))(g, error("")))
  }

  // (implicit MK: Manifest[K], FK: WireFormat[K], MV: Manifest[V], FV: WireFormat[V])
}

object Grouped {
  def apply[K, V](x: DList[(K, Iterable1[V])]): Grouped[K, V] =
    new Grouped[K, V] {
      val list =
        x
    }
}
