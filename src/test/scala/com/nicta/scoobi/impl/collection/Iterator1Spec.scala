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

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.specs2.ScalaCheck
import testing.mutable.UnitSpecification

object Iterator1Data {
  import Iterator1._

  implicit def ArbitraryIterator1[A: Arbitrary]: Arbitrary[Iterator1[A]] =
    Arbitrary(for {
      h <- arbitrary[A]
      t <- arbitrary[List[A]]
      n <- org.scalacheck.Gen.choose(0, t.length + 1)
    } yield {
      val i = h +:: t.iterator
      0 until n foreach (_ => i.next)
      i
    })

}

class Iterator1Spec extends UnitSpecification with ScalaCheck {
  import Iterator1Data._

  "hasNext gives next" >> prop {
    i: Iterator1[Int] =>
      i.next must throwA[NoSuchElementException].iff(!i.hasNext)
  }

  "seq has same hasNext" >> prop {
    i: Iterator1[Int] =>
      i.hasNext == i.seq.hasNext
  }


  "toTraversable produces same head" >> prop {
    i: Iterator1[Int] =>
      i.toTraversable.head == i.first
  }

  "toIterator has same hasNext" >> prop {
    i: Iterator1[Int] =>
      i.hasNext == i.toIterator.hasNext
  }

  "isEmpty gives no next" >> prop {
    i: Iterator1[Int] =>
      i.next must throwA[NoSuchElementException].iff(i.isEmpty)
  }

  "take gives maximum size" >> prop {
    (i: Iterator1[String], n: Int) =>
      (i take n).size <= math.max(0, n)
  }

}
