package com.nicta.scoobi
package core

import impl.collection.Iterable1

sealed trait Grouped[K, V] {
  val list: DList[(K, Iterable1[V])]

  def map[W](f: V => W)(implicit MK: Manifest[K], FK: WireFormat[K], MW: Manifest[W], FW: WireFormat[W]): Grouped[K, W] =
    mapValues(_ map f)

  // foreach

  def :->[W](f: V => W)(implicit MK: Manifest[K], FK: WireFormat[K], MW: Manifest[W], FW: WireFormat[W]): Grouped[K, W] =
    map(f)

  def mapKeys[L](f: K => L)(implicit ML: Manifest[L], FL: WireFormat[L], MV: Manifest[V], FV: WireFormat[V]): Grouped[L, V] =
    Grouped(list map {
      case (k, vs) => (f(k), vs)
    })

  def <-:[L](f: K => L)(implicit ML: Manifest[L], FL: WireFormat[L], MV: Manifest[V], FV: WireFormat[V]): Grouped[L, V] =
    mapKeys(f)

  def mapValues[W](f: Iterable1[V] => Iterable1[W])(implicit MK: Manifest[K], FK: WireFormat[K], MW: Manifest[W], FW: WireFormat[W]): Grouped[K, W] =
    Grouped(list map {
      case (k, vs) => (k, f(vs))
    })

  // bimap, product, ap

  def keys(implicit MK: Manifest[K], FK: WireFormat[K]): DList[K] =
    list map (_._1)

  def values(implicit MW: Manifest[V], FW: WireFormat[V]) =
    list map (_._2)
}

object Grouped {
  def apply[K, V](x: DList[(K, Iterable1[V])]): Grouped[K, V] =
    new Grouped[K, V] {
      val list =
        x
    }
}
