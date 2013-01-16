package com.nicta.scoobi
package core

import scalaz._, Scalaz._, Free._
import org.apache.hadoop.conf.Configuration

import HConfiguration.{Set, Get, Unset}

/**
 * Implements a grammar for [[http://hadoop.apache.org/docs/current/api/org/apache/hadoop/conf/Configuration.html Hadoop configuration]], which is used as the functor giving rise to the free monad [[com.nicta.scoobi.core.HConfigurationInterpreter]].
 *
 * @see [[ftp://ftp.math.mcgill.ca/barr/pdffiles/coeqft.pdf Barr, Michael. "Coequalizers and free triples." Mathematische Zeitschrift 116.4 (1970): 307-322.]]
 * @see [[http://days2012.scala-lang.org/sites/days2012/files/bjarnason_trampolines.pdf Stackless Scala With Free Monads, Rúnar Óli Bjarnason ]]
 *
 * @see [[com.nicta.scoobi.core.HConfigurationInterpreter]]
 * @since 0.7.0
 * @author Tony Morris
 */
sealed trait HConfiguration[+A] {
  /**
   * The functor giving rise to the free monad [[com.nicta.scoobi.core.HConfigurationInterpreter]].
   */
  def map[B](f: A => B): HConfiguration[B] =
    this match {
      case Set(k, v, q) => Set(k, v, f(q))
      case Get(k, q) => Get(k, f compose q)
      case Unset(k, q) => Unset(k, f(q))
    }
}

object HConfiguration extends HConfigurationInstances {
  private[core] case class Set[A](k: String, v: String, q: A) extends HConfiguration[A]
  private[core] case class Get[A](k: String, q: Option[String] => A) extends HConfiguration[A]
  private[core] case class Unset[A](k: String, q: A) extends HConfiguration[A]
}

trait HConfigurationInstances {
  implicit val HConfigurationFunctor: Functor[HConfiguration] =
    new Functor[HConfiguration] {
      def map[A, B](fa: HConfiguration[A])(f: A => B) =
        fa map f
    }
}

/**
 * The free monad produced by the [[com.nicta.scoobi.core.HConfiguration]] functor.
 *
 * @see [[ftp://ftp.math.mcgill.ca/barr/pdffiles/coeqft.pdf Barr, Michael. "Coequalizers and free triples." Mathematische Zeitschrift 116.4 (1970): 307-322.]]
 * @see [[http://days2012.scala-lang.org/sites/days2012/files/bjarnason_trampolines.pdf Stackless Scala With Free Monads, Rúnar Óli Bjarnason ]]
 *
 * @example {{{
 *   val x = 7
 *   val y = 8
 * }}}
 * @see [[com.nicta.scoobi.core.HConfiguration]]
 * @since 0.7.0
 * @author Tony Morris
 */
sealed trait HConfigurationInterpreter[+A] {
  /**
   * The underlying free monad.
   */
  val free: Free[HConfiguration, A]

  def map[B](f: A => B): HConfigurationInterpreter[B] =
    HConfigurationInterpreter(free map f)

  /**
   * The free monad.
   */
  def flatMap[B](f: A => HConfigurationInterpreter[B]): HConfigurationInterpreter[B] =
    HConfigurationInterpreter(free flatMap (f(_).free))

  /** Evaluates a single layer of the free monad. */
  def resume: HConfigurationInterpreterResume[A] =
    free.resume.fold(
      HConfigurationInterpreterResumeCont(_)
    , HConfigurationInterpreterResumeTerm(_)
    )

  /** Changes the configuration functor by the given natural transformation. */
  def hom[G[+_]](f: HConfiguration ~> G)(implicit G: Functor[G]): Free[G, A] =
    free mapSuspension f

  /** Runs a single step, using a function that extracts the resumption from its configuration functor. */
  def bounce[AA >: A](f: HConfiguration[HConfigurationInterpreter[A]] => HConfigurationInterpreter[AA]): HConfigurationInterpreter[AA] =
    HConfigurationInterpreter(free.bounce(x => f(x map (HConfigurationInterpreter(_))).free))

  /** Runs to completion, using a function that extracts the resumption from its configuration functor. */
  def go[AA >: A](f: HConfiguration[HConfigurationInterpreter[AA]] => HConfigurationInterpreter[AA]): AA =
    free.go[AA](x => f(x map (HConfigurationInterpreter(_))).free)

  /** Runs to completion, allowing the resumption function to thread an arbitrary state of type `B`. */
  def goState[B, AA >: A](b: B)(f: (B, HConfiguration[HConfigurationInterpreter[AA]]) => (B, HConfigurationInterpreter[AA])): (B, AA) = {
    val ff: (B, HConfiguration[Free[HConfiguration, AA]]) => (B, Free[HConfiguration, AA]) =
       (b, x) => {
         val (bb, h) = f(b, x map (HConfigurationInterpreter(_)))
         (bb, h.free)
       }

    free.foldRun[B, AA](b)(ff)
  }

  /** Interleave this computation with another, combining the results as a pair. */
  def zip[B](t: HConfigurationInterpreter[B]): HConfigurationInterpreter[(A, B)] =
    HConfigurationInterpreter(free.zipWith(t.free, (a, b: B) => (a, b)))

  /** Interleave this computation with another, combining the results with the given function. */
  def zipWith[B, C](t: HConfigurationInterpreter[B], f: (A, B) => C): HConfigurationInterpreter[C] =
    HConfigurationInterpreter(free.zipWith(t.free, f))

  /** Applies a function in a comonad to the corresponding value in this configuration monad, annihilating both. */
  def zap[G[+_], B](fs: Cofree[G, A => B])(implicit G: Functor[G], d: Zap[HConfiguration, G]): B =
    free zap fs

  /** Applies a function `f` to a value in this configuration monad and a corresponding value in the dual comonad, annihilating both. */
  def zapWith[G[+_], B, C](bs: Cofree[G, B])(f: (A, B) => C)(implicit G: Functor[G], d: Zap[HConfiguration, G]): C =
     free.zapWith(bs)(f)

  /**
   * Run this computation to completion, executing against the given [[http://hadoop.apache.org/docs/current/api/org/apache/hadoop/conf/Configuration.html Hadoop configuration]].
   *
   * '''CAUTION: unsafe operation. Run once only.'''
   */
  @annotation.tailrec
  final def run(c: Configuration): A =
    resume match {
      case HConfigurationInterpreterResumeCont(Set(k, v, q)) =>
        HConfigurationInterpreter({
          c set (k, v)
          q
        }) run c
      case HConfigurationInterpreterResumeCont(Get(k, q)) =>
        HConfigurationInterpreter(q(Option(c get k))) run c
      case HConfigurationInterpreterResumeCont(Unset(k, q)) =>
        HConfigurationInterpreter({
          c unset k
          q
        }) run c
      case HConfigurationInterpreterResumeTerm(a) =>
        a
    }

}

object HConfigurationInterpreter extends HConfigurationFreeFunctions {
  def apply[A](f: Free[HConfiguration, A]): HConfigurationInterpreter[A] =
    new HConfigurationInterpreter[A] {
      val free = f
    }
}

trait HConfigurationFreeFunctions {
  def set[A](k: String, v: String): HConfigurationInterpreter[Unit] =
    HConfigurationInterpreter(Suspend(Set(k, v, Return(()))))

  def get[A](k: String): HConfigurationInterpreter[Option[String]] =
    HConfigurationInterpreter(Suspend(Get(k, Return(_))))

  def unset[A](k: String): HConfigurationInterpreter[Unit] =
    HConfigurationInterpreter(Suspend(Unset(k, Return(()))))
}

/**
 * The result of resuming the configuration free monad.
 *
 * This is either a value (`A`) or a configuration computation (`HConfiguration[Free[HConfiguration, A]]`).
 *
 * @see [[com.nicta.scoobi.core.HConfigurationInterpreter]]
 * @since 0.7.0
 * @author Tony Morris
 */
sealed trait HConfigurationInterpreterResume[+A] {
  def map[B](f: A => B): HConfigurationInterpreterResume[B] =
    this match {
      case HConfigurationInterpreterResumeCont(x) =>
        HConfigurationInterpreterResumeCont(x map (_ map f))
      case HConfigurationInterpreterResumeTerm(a) =>
        HConfigurationInterpreterResumeTerm(f(a))
    }

  def free: HConfigurationInterpreter[A] =
    HConfigurationInterpreter(this match {
      case HConfigurationInterpreterResumeCont(x) =>
        Suspend(x)
      case HConfigurationInterpreterResumeTerm(a) =>
        Return(a)
    })

  def term: Option[A] =
    this match {
      case HConfigurationInterpreterResumeCont(_) =>
        None
      case HConfigurationInterpreterResumeTerm(a) =>
        Some(a)
    }

  def termOr[AA >: A](a: => AA): AA =
    term getOrElse a

  def |[AA >: A](a: => AA): AA =
    termOr(a)

}
private case class HConfigurationInterpreterResumeCont[+A](x: HConfiguration[Free[HConfiguration, A]]) extends HConfigurationInterpreterResume[A]
private case class HConfigurationInterpreterResumeTerm[+A](a: A) extends HConfigurationInterpreterResume[A]

object HConfigurationInterpreterExample {
  def setupConfiguration = {
    val conf = new Configuration()
    conf set ("a", "A")
    conf set ("b", "B")
    conf set ("c", "C")
    conf
  }

  object Before {
    def program {
      val conf = setupConfiguration
      val a = conf get "a"
      function1(conf)
      println("a: " + a)
      println("a: " + (conf get "a"))
      println("b: " + (conf get "b"))
      function2(conf)
      println("a: " + a)
      println("a: " + (conf get "a"))
      println("b: " + (conf get "b"))
      function3(conf)
      println("a: " + a)
      println("a: " + (conf get "a"))
      println("b: " + (conf get "b"))
    }

    def function1(conf: Configuration) {
      println("a: " + (conf get "a"))
      println("b: " + (conf get "b"))
      conf set ("a", "ax")
    }

    def function2(conf: Configuration) {
      println("a: " + (conf get "a"))
      conf set ("b", "bx")
      conf set ("a", "axx")
    }

    def function3(conf: Configuration) {
      conf unset "a"
      println("a: " + (conf get "a"))
      conf unset "b"
      conf set ("a", "axxx")
    }
  }

  object After {

  }

  def main(args: Array[String]) {
    Before.program
  }

}