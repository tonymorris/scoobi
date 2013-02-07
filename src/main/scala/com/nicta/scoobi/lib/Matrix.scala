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
package lib

import Scoobi._
import scala.collection.mutable.ArrayBuffer
import LinearAlgebra._
import core.{Grouped1, WireFormat, Association1}
import WireFormat._

/**
 * A distributed vector, stored in coordinate form.
 * Optimised for a huge spare vector, but should perform reasonably for a large dense vector too.
 * If the vector is small (i.e. a few megabytes or less, you'll probably be better off with the
 * in memory vectors instead
 */
case class DVector[Elem: WireFormat : Ordering, T : WireFormat](data: DList[(Elem, T)]) {
  def byMatrix[V : WireFormat, 
               Q : WireFormat : Ordering](m: DMatrix[Elem, V], mult: (T, V) => Q, add: (Q, Q) => Q): DVector[Elem, Q] = {
    vectorByMatrix(this, m, mult, add)
  }
}

object InMemVector {
  def fromDList[Elem : WireFormat: Ordering, T : WireFormat](in: DList[(Elem, T)]): InMemVector[Elem, T] =
    InMemVector(in.materialise.map(xs => xs.toMap))
}

/**
 * A vector that is small enough to send to one mapper (i.e. a few megs or less). Best suited for a sparse vector
 */
case class InMemVector[Elem: WireFormat : Ordering, T : WireFormat](data: DObject[Map[Elem, T]]) {
  def byMatrix[V : WireFormat, Q: WireFormat](
    m: DColWiseMatrix[Elem, V],
    zero: Q,
    mult: (T, V) => Q,
    add: (Q, Q) => Q): InMemVector[Elem, Q] =
    vectorByMatrix(this, m, zero, mult, add)
}

object InMemDenseVector {
  def fromDList[T : WireFormat](in: DList[(Int, T)], zero: T): InMemDenseVector[T] = {
    InMemDenseVector(in.materialise.map(xs => {
      val buff = scala.collection.mutable.ArrayBuffer[T]()

      xs.foreach {
        case (i, v) => {
          while (buff.length < i + 1) {
            buff.append(zero)
          }
          buff.insert(i, v)
        }
      }
      buff.toIndexedSeq
    }))
  }

}

/**
 * An efficient, dense vector that needs to be small enough to fit in memory. It must be indexed by an int
 */
case class InMemDenseVector[T : WireFormat](data: DObject[IndexedSeq[T]]) {
  def byMatrix[V : WireFormat, Q : WireFormat](
    m: DColWiseMatrix[Int, V],
    zero: Q,
    mult: (T, V) => Q,
    add: (Q, Q) => Q): InMemDenseVector[Q] =
    vectorByMatrix(this, m, zero, mult, add)
}

/**
 * A distributed row-wise matrix. This is an efficient representation for multiplying by an in-memory vector. The contents of each row
 * must be small enough to fit in memory
 */
case class DRowWiseMatrix[Elem: WireFormat : Ordering, T : WireFormat](data: Grouped1[Elem, (Elem, T)]) {
  
  def byVector[V, R](
    dv: InMemDenseVector[V],
    zero: R,
    mult: (T, V) => R,
    add: (R, R) => R)(implicit ev: DRowWiseMatrix[Elem, T] <:< DRowWiseMatrix[Int, T],
      vw: WireFormat[V],
      rw: WireFormat[R]): InMemDenseVector[R] = matrixByVector(this, dv, zero, mult, add)
      
  def byVector[V : WireFormat, R : WireFormat](
    dv: InMemVector[Elem, V],
    mult: (T, V) => R,
    add: (R, R) => R): InMemVector[Elem, R] = matrixByVector(this, dv, mult, add)

  def col: DColWiseMatrix[Elem, T] =
    DColWiseMatrix(data)
}

/**
 * A col-wise matrix. This is an efficient representation for multiplying by an in-memory vector. The contents of each column
 * must be small enough to fit in memory
 */
case class DColWiseMatrix[Elem, T](data: core.Grouped1[Elem, (Elem, T)]) {
  def row(implicit E: WireFormat[Elem], O: Ordering[Elem], W: WireFormat[T]): DRowWiseMatrix[Elem, T] =
    DRowWiseMatrix(data)
}

/**
 * A distributed Matrix, stored in coordinate format.
 * Operations are optimised assuming the matrix is both large and sparse, but shouldn't be too terrible for large dense matrixes.
 */
case class DMatrix[Elem : WireFormat: Ordering, Value : WireFormat](data: DList[((Elem, Elem), Value)]) {

  def byMatrix[V : WireFormat, Q : WireFormat](
    r: DMatrix[Elem, V],
    mult: (Value, V) => Q,
    add: (Q, Q) => Q): DMatrix[Elem, Q] = matrixByMatrix(data, r, mult, add)

  /* triggers an expensive conversion */
  def byVector[V, R](
    dv: InMemDenseVector[V],
    zero: R,
    mult: (Value, V) => R,
    add: (R, R) => R)(
      implicit ev: DList[((Elem, Elem), Value)] <:< DList[((Int, Int), Value)],
      vm: WireFormat[V],
      rm: WireFormat[R]): InMemDenseVector[R] = {

    val tmp: DList[((Int, Int), Value)] = data

    matrixByVector(tmp, dv, zero, mult, add) // TODO: use 'this' ?
  }

  def byVector[V : WireFormat : Ordering, R : WireFormat: Ordering](
    dv: InMemVector[Elem, V],
    mult: (Value, V) => R,
    add: (R, R) => R): InMemVector[Elem, R] =
    matrixByVector(this, dv, mult, add)
    
    
  def byVector[V : WireFormat, Q: WireFormat : Ordering](
    v: DVector[Elem, V],
    mult: (Value, V) => Q,
    add: (Q, Q) => Q): DVector[Elem, Q] = matrixByVector(this, v, mult, add)


  def transpose: DMatrix[Elem, Value] = DMatrix(this.data map { case ((r, c), v) => ((c, r), v) })
}

object LinearAlgebra {

  /* Does an expensive conversion */
  def matrixByVector[T : WireFormat, V : WireFormat, R : WireFormat](
    m: DMatrix[Int, T],
    dv: InMemDenseVector[V],
    zero: R,
    mult: (T, V) => R,
    add: (R, R) => R): InMemDenseVector[R] = matrixByVector(m, dv, zero, mult, add)

  def matrixByVector[T, V, R](
    m: DRowWiseMatrix[Int, T],
    dv: InMemDenseVector[V],
    zero: R,
    mult: (T, V) => R,
    add: (R, R) => R)(implicit tm: WireFormat[T],
      vm: WireFormat[V],
      rm: WireFormat[R]): InMemDenseVector[R] = {
    val all = dv join m.data.list

    val distributedVector =
      all.map {
        case (arr, a) => {

          val products =
            for (q <- a.values if arr.contains(q._1))
              yield mult(q._2, arr(q._1))

          val result = if (products.isEmpty) zero else products.reduce(add)

          (a.key, result)
        }
      }

    InMemDenseVector.fromDList(distributedVector, zero)
  }

  def matrixByVector[Elem : WireFormat: Ordering, T : WireFormat, V: WireFormat, R: WireFormat](
    m: DRowWiseMatrix[Elem, T],
    dv: InMemVector[Elem, V],
    mult: (T, V) => R,
    add: (R, R) => R): InMemVector[Elem, R] = {

    val all = dv join m.data.list

    val distributedVector =
      all.mapFlatten {
        case (arr, a) => {

          val products =
            for (q <- a.values if arr.contains(q._1))
              yield mult(q._2, arr(q._1))

          if (products.isEmpty) None else Some(a.key, products.reduce(add))
        }
      }

    InMemVector.fromDList(distributedVector)
  }

  def vectorByMatrix[T: WireFormat, V: WireFormat, R: WireFormat](
    dv: InMemDenseVector[V],
    m: DColWiseMatrix[Int, T],
    zero: R,
    mult: (V, T) => R,
    add: (R, R) => R): InMemDenseVector[R] = matrixByVector(m.row, dv, zero, (a: T, b: V) => mult(b, a), add)

  def vectorByMatrix[Elem: WireFormat: Ordering, T: WireFormat, V: WireFormat, R: WireFormat](
    dv: InMemVector[Elem, T],
    m: DColWiseMatrix[Elem, V],
    zero: R,
    mult: (T, V) => R,
    add: (R, R) => R): InMemVector[Elem, R] = matrixByVector(m.row, dv, (a: V, b: T) => mult(b, a), add)

  /* Does an expensive conversion */
  def vectorByMatrix[T: WireFormat, V: WireFormat, R: WireFormat](
    dv: InMemDenseVector[V],
    m: DMatrix[Int, T],
    zero: R,
    mult: (V, T) => R,
    add: (R, R) => R): InMemDenseVector[R] = vectorByMatrix(dv, m, zero, mult, add)

  def matrixBySparseFunc[Elem: WireFormat: Ordering, V: WireFormat, Value: WireFormat, Q: WireFormat](
    matrix: DMatrix[Elem, Value],
    generateRow: () => Map[Elem, V],
    mult: (Value, V) => Q,
    add: (Q, Q) => Q): DMatrix[Elem, Q] =
    {
      val left = matrix.by(_._1._2)

      left.groupByKey.parallelDo(new BasicDoFn[Association1[Elem, ((Elem, Elem), Value)], ((Elem, Elem), Q)] {
        def process(input: Association1[Elem, ((Elem, Elem), Value)], emitter: Emitter[((Elem, Elem), Q)]) {
          val bs = generateRow()

          for (a <- input.values) {
            bs.foreach {
              b => emitter.emit(((a._1._1, b._1), mult(a._2, b._2)))
            }
          }
        }
      }).groupByKey.combine((a: Q, b: Q) => add(a, b))
    }

  def matrixByDenseFunc[V: WireFormat, Value: WireFormat, Q: WireFormat](
    matrix: DMatrix[Int, Value],
    generateRow: () => Seq[V],
    mult: (Value, V) => Q,
    add: (Q, Q) => Q): DMatrix[Int, Q] =
    {
      val left = matrix.map(a => (a._1._2, (a._1._1,a._2)))

      left.groupByKey.parallelDo(
        new BasicDoFn[Association1[Int, (Int, Value)], ((Int, Int), Q)] {
          def process(input: Association1[Int, (Int, Value)], emitter: Emitter[((Int, Int), Q)]) {
            val bs = generateRow()

            for (a <- input.values) {
              bs.zipWithIndex.foreach {
                b => emitter.emit(((a._1, b._2), mult(a._2, b._1)))
              }
            }
          }
        }).groupByKey.combine((a: Q, b: Q) => add(a, b))

    }

  def matrixByMatrix[Elem: WireFormat: Ordering, V: WireFormat, Value: WireFormat, Q: WireFormat](
    l: DMatrix[Elem, Value],
    r: DMatrix[Elem, V],
    mult: (Value, V) => Q,
    add: (Q, Q) => Q): DMatrix[Elem, Q] = {

    val left = l.by(_._1._2).map(x => (x._1, Left(x._2): Either[((Elem, Elem), Value), ((Elem, Elem), V)]))
    val right = r.by(_._1._1).map(x => (x._1, Right(x._2): Either[((Elem, Elem), Value), ((Elem, Elem), V)]))

    (left ++ right).groupByKey.parallelDo(new BasicDoFn[Association1[Elem, Either[((Elem, Elem), Value), ((Elem, Elem), V)]], ((Elem, Elem), Q)] {
      def process(input: Association1[Elem, Either[((Elem, Elem), Value), ((Elem, Elem), V)]], emitter: Emitter[((Elem, Elem), Q)]) {
        val as: ArrayBuffer[((Elem, Elem), Value)] = new ArrayBuffer[((Elem, Elem), Value)]()
        val bs: ArrayBuffer[((Elem, Elem), V)] = new ArrayBuffer[((Elem, Elem), V)]()

        input.values foreach {
          case Left(a) => {
            as += a
            bs.foreach {
              b => emitter.emit((a._1._1, b._1._2), mult(a._2, b._2))
            }
          }
          case Right(b) => {
            bs += b
            as.foreach {
              a => emitter.emit((a._1._1, b._1._2), mult(a._2, b._2))
            }
          }
        }
      }
    }).combine[(Elem, Elem), Q]((a: Q, b: Q) => add(a, b))
  }

  def matrixByVector[Elem: WireFormat: Ordering, V: WireFormat, Value: WireFormat, Q: WireFormat: Ordering](
    l: DMatrix[Elem, Value],
    r: DVector[Elem, V],
    mult: (Value, V) => Q,
    add: (Q, Q) => Q): DVector[Elem, Q] = matrixByMatrix(l, r.map(x => ((x._1, x._1), x._2)), mult, add).map(x => (x._1._2, x._2))

  def vectorByMatrix[Elem: WireFormat: Ordering, V: WireFormat, Value: WireFormat, Q: WireFormat : Ordering](
    l: DVector[Elem, Value],
    r: DMatrix[Elem, V],
    mult: (Value, V) => Q,
    add: (Q, Q) => Q): DVector[Elem, Q] = matrixByMatrix(l.map(x => ((x._1, x._1), x._2)), r, mult, add).map(x => (x._1._1, x._2))

  // work around a hadoop bug with combiners timing out...
  def matrixByMatrixTimeoutWorkaround[Elem: WireFormat: Ordering, V: WireFormat, Value: WireFormat, Q: WireFormat](
    l: DMatrix[Elem, Value],
    r: DMatrix[Elem, V],
    mult: (Value, V) => Q,
    add: (Q, Q) => Q): DMatrix[Elem, Q] =
    (l.by(_._1._2) join r.by(_._1._1))
      .map { case (_, (a, b)) => ((a._1._1, b._1._2), mult(a._2, b._2)) }
      .groupByKey
      .combine(add)
}

