package com.nicta.scoobi
package core

import impl.collection.Iterable1

sealed trait Grouped[K, V] {
  val list: DList[Association1[K, V]]

  def mapValues[W](f: Iterable1[V] => Iterable1[W])(implicit MK: Manifest[K], FK: WireFormat[K], MW: Manifest[W], FW: WireFormat[W]): Grouped[K, W] =
    Grouped(list map (_ mapValues f))

  def map[W](f: V => W)(implicit MK: Manifest[K], FK: WireFormat[K], MW: Manifest[W], FW: WireFormat[W]): Grouped[K, W] =
    Grouped(list map (_ map f))

  def :->[W](f: V => W)(implicit MK: Manifest[K], FK: WireFormat[K], MW: Manifest[W], FW: WireFormat[W]): Grouped[K, W] =
    map(f)

  def mapKeys[L](f: K => L)(implicit ML: Manifest[L], FL: WireFormat[L], MV: Manifest[V], FV: WireFormat[V]): Grouped[L, V] =
    Grouped(list map (_ mapKey f))

  def <-:[L](f: K => L)(implicit ML: Manifest[L], FL: WireFormat[L], MV: Manifest[V], FV: WireFormat[V]): Grouped[L, V] =
    mapKeys(f)

  def keys(implicit MK: Manifest[K], FK: WireFormat[K]): DList[K] =
    list map (_.key)

  def values(implicit MW: Manifest[V], FW: WireFormat[V]): DList[Iterable1[V]] =
    list map (_.values)

  def valuesF(implicit MW: Manifest[V], FW: WireFormat[V]): DList[V] =
    list flatMap (_.values.toIterable)

  def bimap[L, W](f: K => L, g: V => W)(implicit ML: Manifest[L], FL: WireFormat[L], MW: Manifest[W], FW: WireFormat[W]): Grouped[L, W] =
    Grouped(list map (_ bimap (f, g)))


  // product, ap

}

object Grouped {
  def apply[K, V](x: DList[Association1[K, V]]): Grouped[K, V] =
    new Grouped[K, V] {
      val list =
        x
    }
}
