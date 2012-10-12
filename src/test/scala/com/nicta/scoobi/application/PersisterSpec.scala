package com.nicta.scoobi
package application

import Scoobi._
import testing.{NictaHadoop, TempFiles}
import testing.mutable.SimpleJobs
import testing.TestFiles._
import sys.process._
import shapeless._
import HList._
import TypeOperators._
import UnaryTCConstraint._

class PersisterSpec extends NictaHadoop with SimpleJobs {
  /*
  "A sequence of DLists can be persisted simultaneously to text files" >> { implicit sc: ScoobiConfiguration =>
    val dirs = Seq.fill(3)(TempFiles.createTempDir("test"))
    val lists = Seq(DList(1, 2), DList(3, 4), DList(5, 6)).zip(dirs).map { case (l, d) => toTextFile(l, path(d)) }
    persist(lists)
    dirs.map(dirResults).flatten.toSet must_== Set("1","2","3","4","5","6")
  }
  "A tuple containing a sequence of DLists can be persisted to text files" >> { implicit sc: ScoobiConfiguration =>
    val dirs = Seq.fill(4)(TempFiles.createTempDir("test"))
    val lists = Seq(DList(1, 2), DList(3, 4), DList(5, 6)).zip(dirs).map { case (l, d) => toTextFile(l, path(d)) }
    val firstList = toTextFile(DList(7, 8, 9), path(dirs.last))
    persist((firstList, lists))
    dirs.map(dirResults).flatten.toSet must_== Set("1","2","3","4","5","6","7","8","9")
  }
  "A sequence of DLists can be persisted simultaneously with the run method" >> { implicit sc: ScoobiConfiguration =>
    val lists = Seq(DList(1, 2), DList(3, 4), DList(5, 6))
    lists.run.flatten.toSet must_== Set(1,2,3,4,5,6)
  }
*/
  "All the methods for persisting data can be mixed together" >> { implicit sc: ScoobiConfiguration =>
    Seq("xs", "ass0", "ass1") foreach { f => stringToProcess("rm -r "+f)! }
    val xs  = DList(1, 2)
    val y   = DList({println("============EVALUATING 1 =========");1}, 2).sum
    val z   = DList(1, 2).sum
    val ass = Seq(xs, DList(3, 4)).zipWithIndex map { case (as, ix) => toTextFile(as, "ass" + ix) }
    val bs  = Seq(y, z)

    persist(toTextFile(xs, "xs") :: y :: z :: ass :: bs :: HNil)(hlistUnaryTC1)
    //persist((toTextFile(xs, "xs"), y, z, ass, bs))
    ok
  }
  import ToList._
  def persist[L <: HList : *->*[Persistable]#λ](list: L) = list.toString
}
