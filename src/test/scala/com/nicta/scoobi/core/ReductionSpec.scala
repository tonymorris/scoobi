package com.nicta.scoobi
package core

import org.specs2.ScalaCheck
import com.nicta.scoobi.testing.mutable.UnitSpecification

import scalaz._, Scalaz._

class ReductionSpec extends UnitSpecification with ScalaCheck {
  import Reduction._

  "construct => reduce" >> prop {
      (f: (Int, Int) => Int, x: Int, y: Int) =>
        Reduction(f).reduce(x, y) == f(x, y)
    }

  "constant" >> prop {
      (f: Int, x: Int, y: Int) =>
        constant(f).reduce(x, y) == f
    }

  "split1" >> prop {
      (f: Int => Int, x: Int, y: Int) =>
        split1(f).reduce(x, y) == f(x)
    }

  "split2" >> prop {
      (f: Int => Int, x: Int, y: Int) =>
        split2(f).reduce(x, y) == f(y)
    }

  "first" >> prop {
      (x: Int, y: Int) =>
        first[Int].reduce(x, y) == x
    }

  "last" >> prop {
      (x: Int, y: Int) =>
        last[Int].reduce(x, y) == y
    }

  "first associative" >> prop {
      (x: Int, y: Int, z: Int) =>
        first[Int].associative.law(x, y, z)
    }

  "last associative" >> prop {
      (x: Int, y: Int, z: Int) =>
        last[Int].associative.law(x, y, z)
    }

  "firstOption associative" >> prop {
      (x: Option[Int], y: Option[Int], z: Option[Int]) =>
        firstOption[Int].associative.law(x, y, z)
    }

}
