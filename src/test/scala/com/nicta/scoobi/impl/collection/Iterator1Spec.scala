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

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Properties}

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

object Iterator1Spec extends Properties("Iterator1") {
  import Iterator1Data._

  property("hasNext gives next") =
    forAll((i: Iterator1[Int]) => {
      val q = i.hasNext
      try {
        i.next
        q
      } catch {
        case e: NoSuchElementException => !q
      }
    })

}
