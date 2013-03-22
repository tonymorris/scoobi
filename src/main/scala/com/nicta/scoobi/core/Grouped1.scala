package com.nicta.scoobi
package core

/**
 * A distributed list of associations.
 *
 * @see [[com.nicta.scoobi.core.Association1]]
 */
sealed trait Grouped1[K, V] {
  /**
   * The underlying distributed list.
   */
  val list: DList[Association1[K, V]]

  /**
   * Deconstruct the association to a pair.
   */
  def paired(implicit WK: WireFormat[K], WV: WireFormat[V]): DList[(K, Iterable1[V])] =
    list map (_.paired)

  /** make a Grouped1 runnable, executing the computation and returning the values */
  def run(implicit configuration: core.ScoobiConfiguration): Seq[Association1[K, V]] = {
    import com.nicta.scoobi.Scoobi._
    error("") // Llist.run
  }

  /**
   * Run a function on the values of the distributed list to produce new values.
   */
  def mapValues[W](f: Iterable1[V] => Iterable1[W])(implicit fk: WireFormat[K], fw: WireFormat[W]): Grouped1[K, W] =
    Grouped1(list map (_ mapValues f))

  /**
   * Run a function on each value in the distributed list to produce a distributed list with new values. Synonym for `:->`.
   */
  def map[W](f: V => W)(implicit fk: WireFormat[K], fw: WireFormat[W]): Grouped1[K, W] =
    Grouped1(list map (_ map f))

  /**
   * Run a function on each value in the distributed list to produce a distributed list with new values. Synonym for `:->`.
   */
  def mapa[W, X](f: Association1[K, V] => Association1[W, X])(implicit fk: WireFormat[K], fw: WireFormat[W], fx: WireFormat[X]): Grouped1[W, X] =
    Grouped1(list map f)

  /**
   * Run a function on each value in the distributed list to produce a distributed list with new values. Synonym for `map`.
   */
  def :->[W](f: V => W)(implicit fk: WireFormat[K], fw: WireFormat[W]): Grouped1[K, W] =
    map(f)

  /**
   * Run a function on each key in the distributed list to produce a distributed list with new key. Synonym for `<-:`.
   */
  def mapKeys[L](f: K => L)(implicit fl: WireFormat[L], fv: WireFormat[V]): Grouped1[L, V] =
    Grouped1(list map (_ mapKey f))

  /**
   * Run a function on each key in the distributed list to produce a distributed list with new key. Synonym for `mapKeys`.
   */
  def <-:[L](f: K => L)(implicit fl: WireFormat[L], fv: WireFormat[V]): Grouped1[L, V] =
    mapKeys(f)

  /**
   * The keys of the underlying distributed list.
   */
  def keys(implicit fk: WireFormat[K]): DList[K] =
    list map (_.key)

  /**
   * The values of the underlying distributed list grouped by their key.
   */
  def values(implicit fv: WireFormat[V]): DList[Iterable1[V]] =
    list map (_.values)

  /**
   * The values of the underlying distributed list flattened.
   */
  def valuesF(implicit fv: WireFormat[V]): DList[V] =
    list mapFlatten (_.values.toIterable)

  /**
   * Map two functions on the keys and values (binary map) of the distributed list.
   */
  def bimap[L, W](f: K => L, g: V => W)(implicit fl: WireFormat[L], fw: WireFormat[W]): Grouped1[L, W] =
    Grouped1(list map (_ bimap (f, g)))

  def parallelDo[B : WireFormat](dofn: DoFn[Association1[K, V], B]): DList[B] =
    list parallelDo dofn

  // todo DList1
  def combine(f: (V, V) => V)(implicit wk: WireFormat[K], wv: WireFormat[V]): DList[(K, V)] =
    list combine f

  def materialise: DObject[Iterable[Association1[K, V]]]  =
    list.materialise

  def filter(p: Association1[K, V] => Boolean): Grouped1[K, V] =
    Grouped1(list filter p)

  import impl.plan.DListImpl
  import impl.plan.comp.GroupByKey
  import WireFormat.wireFormat

  def groupByKey(implicit wk: WireFormat[K], gpk: Grouping[K], wv: WireFormat[V]): Grouped1[K, Iterable1[V]] =
    Grouped1(new DListImpl(GroupByKey(list.getComp, wk, gpk, wv)))

  def ++(g: Grouped1[K, V]): Grouped1[K, V] =
    Grouped1(list ++ g.list)
}

object Grouped1 {
  /**
   * Construct a `Grouped1` with the given distributed list.
   */
  def apply[K, V](x: DList[Association1[K, V]]): Grouped1[K, V] =
    new Grouped1[K, V] {
      val list = x
    }
}
