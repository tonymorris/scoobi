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
// A use-case to demonstrate the hadoop configuration interpreter.
// The use-case deliberately intersperses effects (e.g. println) that are unrelated to hadoop configuration.
// This requires the construction of our own interpreter for these effects on top of the existing hadoop interpreter.
object HConfigurationInterpreterExample {
  // A hadoop configuration
  def setupConfiguration = {
    val conf = new Configuration()
    conf set ("a", "A")
    conf set ("b", "B")
    conf set ("c", "C")
    conf
  }

  // A typical side-effectful program that uses a hadoop configuration and intersperses arbitrary side-effects.
  // Two effects are interspersed in this example; println (stdout) and err.println (stderr).
  // The ultimate goal is mechanically transform the program below into pure functional code using the hadoop interpreter.
  // This transformation is performed in `object After`.
  object Before {
    // The top-level program.
    def program {
      val conf = setupConfiguration
      val a = conf get "a"
      function1(conf)
      println("a: " + a)
      println("a: " + (conf get "a"))
      println("b: " + (conf get "b"))
      function2(conf)
      println("a: " + a)
      Console.err.println("a: " + (conf get "a"))
      println("b: " + (conf get "b"))
      function3(conf)
      println("a: " + a)
      Console.err.println("a: " + (conf get "a"))
      println("b: " + (conf get "b"))
    }

    // A sub-program.
    def function1(conf: Configuration) {
      println("a: " + (conf get "a"))
      Console.err.println("b: " + (conf get "b"))
      conf set ("a", "ax")
    }

    // A sub-program.
    def function2(conf: Configuration) {
      println("a: " + (conf get "a"))
      conf set ("b", "bx")
      conf set ("a", "axx")
    }

    // A sub-program.
    def function3(conf: Configuration) {
      conf unset "a"
      println("a: " + (conf get "a"))
      conf unset "b"
      conf set ("a", "axxx")
    }
  }

  // The transformation of the `object Before` program using the hadoop interpreter.
  object After {
    // Set up a grammar to build on top of the hadoop interpreter. A minimum requirement is that a `Functor` is provided.
    // To properly model the `Before` program, this grammar accepts one of the following:
    // * A hadoop configuration interpreter
    // * A println (out) effect
    // * A println (err) effect
    sealed trait HConfigurationEffect[+A] {
      // The `map` method that is used for the `Functor` which gives the free monad (an arbitrary interpreter).
      def map[B](f: A => B): HConfigurationEffect[B] =
        this match {
          case HConfigurationNoEffect(x) => HConfigurationNoEffect(x map f)
          case HConfigurationOutPrintlnEffect(s, a) => HConfigurationOutPrintlnEffect(s, f(a))
          case HConfigurationErrPrintlnEffect(s, a) => HConfigurationErrPrintlnEffect(s, f(a))
        }
    }
    case class HConfigurationNoEffect[+A](x: HConfigurationInterpreter[A]) extends HConfigurationEffect[A]
    case class HConfigurationOutPrintlnEffect[+A](s: String, a: A) extends HConfigurationEffect[A]
    case class HConfigurationErrPrintlnEffect[+A](s: String, a: A) extends HConfigurationEffect[A]

    object HConfigurationEffect {
      // The actual functor instance, which simply calls map
      implicit val HConfigurationEffectFunctor: Functor[HConfigurationEffect] =
        new Functor[HConfigurationEffect] {
          def map[A, B](fa: HConfigurationEffect[A])(f: A => B) =
            fa map f
        }
    }

    // An interpreter for the hadoop configuration interpreter with effects. A functor must be supplied, which simply uses the underlying `Free`, which is a functor since the grammar is a functor.
    case class HConfigurationEffectInterpreter[+A](free: Free[HConfigurationEffect, A]) {
      // The `map` method that is used for the `Functor` which gives the free monad (an arbitrary interpreter).
      def map[B](f: A => B): HConfigurationEffectInterpreter[B] =
        HConfigurationEffectInterpreter(free map f)

      // Interpreters are monads.
      def flatMap[B](f: A => HConfigurationEffectInterpreter[B]): HConfigurationEffectInterpreter[B] =
        HConfigurationEffectInterpreter(free flatMap (f(_).free))

      // Unsafe operation that runs all effects on the given configuration.
      // This operation is unsafe in that when it is called, the equational reasoning property are lost.
      // However, the boundary within which that property is lost is clearly delineated.
      // This is not a pure function, but it isolates side-effects.
      @annotation.tailrec
      final def run(c: Configuration): A =
        free.resume match {
          // run the hadoop configuration interpreter then continue
          case -\/(HConfigurationNoEffect(i)) =>
            HConfigurationEffectInterpreter(i run c) run c
          // run a println (out) effect then continue.
          case -\/(HConfigurationOutPrintlnEffect(s, a)) =>
            HConfigurationEffectInterpreter({
              println(s)
              a
            }) run c
          // run a println (err) effect then continue.
          case -\/(HConfigurationErrPrintlnEffect(s, a)) =>
            HConfigurationEffectInterpreter({
              Console.err.println(s)
              a
            }) run c
          // halt the recursion and produce a value.
          case \/-(a) =>
            a
        }
    }

    object HConfigurationEffectInterpreter {
      // The functor is required for the interpreter.
      implicit val HConfigurationEffectInterpreterFunctor: Functor[HConfigurationEffectInterpreter] =
        new Functor[HConfigurationEffectInterpreter] {
          def map[A, B](fa: HConfigurationEffectInterpreter[A])(f: A => B) =
            fa map f
        }

      // Used internally (see below).
      private def lift[A](x: HConfigurationInterpreter[A]): HConfigurationEffectInterpreter[A] =
        HConfigurationEffectInterpreter(x hom (new (HConfiguration ~> HConfigurationEffect) {
          def apply[X](c: HConfiguration[X]) =
            HConfigurationNoEffect(HConfigurationInterpreter(Suspend(c map (Return(_)))))
        }))

      // Interpreter instruction to set a key/value on the hadoop configuration.
      def set[A](k: String, v: String): HConfigurationEffectInterpreter[Unit] =
        lift(HConfigurationInterpreter.set(k, v))

      // Interpreter instruction to get key's value from the hadoop configuration.
      def get[A](k: String): HConfigurationEffectInterpreter[Option[String]] =
        lift(HConfigurationInterpreter.get(k))

      // Interpreter instruction to unset a key on the hadoop configuration.
      def unset[A](k: String): HConfigurationEffectInterpreter[Unit] =
        lift(HConfigurationInterpreter.unset(k))

      // Interpreter instruction to print (out) the given string.
      def outprintln[A](s: String): HConfigurationEffectInterpreter[Unit] =
        HConfigurationEffectInterpreter(Suspend(HConfigurationOutPrintlnEffect(s, Return(()))))

      // Interpreter instruction to print (err) the given string.
      def errprintln[A](s: String): HConfigurationEffectInterpreter[Unit] =
        HConfigurationEffectInterpreter(Suspend(HConfigurationErrPrintlnEffect(s, Return(()))))

    }

    import HConfigurationEffectInterpreter._

    // A top-level program.
    def program = {
      // A top-level pure-functional program.
      val p =
        for {
          a <- get("a")
          _ <- function1
          _ <- outprintln("a: " + a)
          aa <- get("a")
          _ <- outprintln("a: " + aa)
          b <- get("b")
          _ <- outprintln("b: " + b)
          _ <- function2
          _ <- outprintln("a: " + a)
          aaa <- get("a")
          _ <- outprintln("a: " + aaa)
          bb <- get("b")
          _ <- outprintln("b: " + bb)
          _ <- function3
          _ <- outprintln("a: " + a)
          aaaa <- get("a")
          _ <- errprintln("a: " + aaaa)
          bbb <- get("b")
          _ <- outprintln("b: " + bbb)
        } yield ()
      val conf = setupConfiguration
      // Invoke the unsafe operation (and then forget the configuration ever existed).
      p run conf
    }

      /*
    // A pure-functional sub-program.
    def function1 =
      for {
        a <- get("a")
        _ <- outprintln("a: " + a)
        b <- get("b")
        _ <- errprintln("b: " + b)
        _ <- set ("a", "ax")
      } yield ()

    // A pure-functional sub-program.
    def function2 =
      for {
        a <- get("a")
        _ <- outprintln("a: " + a)
        _ <- set ("b", "bx")
        _ <- set ("a", "axx")
      } yield ()

    // A pure-functional sub-program.
    def function3 =
      for {
        _ <- unset("a")
        a <- get("a")
        _ <- outprintln("a: " + a)
        _ <- unset("b")
        _ <- set ("a", "axxx")
      } yield ()
      */
  }

  // Call the `Before` and `After` programs.
  def main(args: Array[String]) {
    Before.program
    println("====")
    After.program
  }
}
}}}
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
    free.resume match {
      case -\/(e) => HConfigurationInterpreterResumeCont(e)
      case \/-(r) => HConfigurationInterpreterResumeTerm(r)
    }

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

object HConfigurationInterpreter extends HConfigurationInterpreterFunctions {
  def apply[A](f: Free[HConfiguration, A]): HConfigurationInterpreter[A] =
    new HConfigurationInterpreter[A] {
      val free = f
    }
}

trait HConfigurationInterpreterFunctions {
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
case class HConfigurationInterpreterResumeCont[+A](x: HConfiguration[Free[HConfiguration, A]]) extends HConfigurationInterpreterResume[A]
case class HConfigurationInterpreterResumeTerm[+A](a: A) extends HConfigurationInterpreterResume[A]

// A use-case to demonstrate the hadoop configuration interpreter.
// The use-case deliberately intersperses effects (e.g. println) that are unrelated to hadoop configuration.
// This requires the construction of our own interpreter for these effects on top of the existing hadoop interpreter.
object HConfigurationInterpreterExample {
  // A hadoop configuration
  def setupConfiguration = {
    val conf = new Configuration()
    conf set ("a", "A")
    conf set ("b", "B")
    conf set ("c", "C")
    conf
  }

  // A typical side-effectful program that uses a hadoop configuration and intersperses arbitrary side-effects.
  // Two effects are interspersed in this example; println (stdout) and err.println (stderr).
  // The ultimate goal is mechanically transform the program below into pure functional code using the hadoop interpreter.
  // This transformation is performed in `object After`.
  object Before {
    // The top-level program.
    def program {
      val conf = setupConfiguration
      val a = conf get "a"
      function1(conf)
      println("a: " + a)
      println("a: " + (conf get "a"))
      println("b: " + (conf get "b"))
      function2(conf)
      println("a: " + a)
      Console.err.println("a: " + (conf get "a"))
      println("b: " + (conf get "b"))
      function3(conf)
      println("a: " + a)
      Console.err.println("a: " + (conf get "a"))
      println("b: " + (conf get "b"))
    }

    // A sub-program.
    def function1(conf: Configuration) {
      println("a: " + (conf get "a"))
      Console.err.println("b: " + (conf get "b"))
      conf set ("a", "ax")
    }

    // A sub-program.
    def function2(conf: Configuration) {
      println("a: " + (conf get "a"))
      conf set ("b", "bx")
      conf set ("a", "axx")
    }

    // A sub-program.
    def function3(conf: Configuration) {
      conf unset "a"
      println("a: " + (conf get "a"))
      conf unset "b"
      conf set ("a", "axxx")
    }
  }

  // The transformation of the `object Before` program using the hadoop interpreter.
  object After {
    // Set up a grammar to build on top of the hadoop interpreter. A minimum requirement is that a `Functor` is provided.
    // To properly model the `Before` program, this grammar accepts one of the following:
    // * A hadoop configuration interpreter
    // * A println (out) effect
    // * A println (err) effect
    sealed trait HConfigurationEffect[+A] {
      // The `map` method that is used for the `Functor` which gives the free monad (an arbitrary interpreter).
      def map[B](f: A => B): HConfigurationEffect[B] =
        this match {
          case HConfigurationNoEffect(x) => HConfigurationNoEffect(x map f)
          case HConfigurationOutPrintlnEffect(s, a) => HConfigurationOutPrintlnEffect(s, f(a))
          case HConfigurationErrPrintlnEffect(s, a) => HConfigurationErrPrintlnEffect(s, f(a))
        }
    }
    case class HConfigurationNoEffect[+A](x: HConfigurationInterpreter[A]) extends HConfigurationEffect[A]
    case class HConfigurationOutPrintlnEffect[+A](s: String, a: A) extends HConfigurationEffect[A]
    case class HConfigurationErrPrintlnEffect[+A](s: String, a: A) extends HConfigurationEffect[A]

    object HConfigurationEffect {
      // The actual functor instance, which simply calls map
      implicit val HConfigurationEffectFunctor: Functor[HConfigurationEffect] =
        new Functor[HConfigurationEffect] {
          def map[A, B](fa: HConfigurationEffect[A])(f: A => B) =
            fa map f
        }
    }

    // An interpreter for the hadoop configuration interpreter with effects. A functor must be supplied, which simply uses the underlying `Free`, which is a functor since the grammar is a functor.
    case class HConfigurationEffectInterpreter[+A](free: Free[HConfigurationEffect, A]) {
      // The `map` method that is used for the `Functor` which gives the free monad (an arbitrary interpreter).
      def map[B](f: A => B): HConfigurationEffectInterpreter[B] =
        HConfigurationEffectInterpreter(free map f)

      // Interpreters are monads.
      def flatMap[B](f: A => HConfigurationEffectInterpreter[B]): HConfigurationEffectInterpreter[B] =
        HConfigurationEffectInterpreter(free flatMap (f(_).free))

      // Unsafe operation that runs all effects on the given configuration.
      // This operation is unsafe in that when it is called, the equational reasoning property are lost.
      // However, the boundary within which that property is lost is clearly delineated.
      // This is not a pure function, but it isolates side-effects.
      @annotation.tailrec
      final def run(c: Configuration): A =
        free.resume match {
          // run the hadoop configuration interpreter then continue
          case -\/(HConfigurationNoEffect(i)) =>
            HConfigurationEffectInterpreter(i run c) run c
          // run a println (out) effect then continue.
          case -\/(HConfigurationOutPrintlnEffect(s, a)) =>
            HConfigurationEffectInterpreter({
              println(s)
              a
            }) run c
          // run a println (err) effect then continue.
          case -\/(HConfigurationErrPrintlnEffect(s, a)) =>
            HConfigurationEffectInterpreter({
              Console.err.println(s)
              a
            }) run c
          // halt the recursion and produce a value.
          case \/-(a) =>
            a
        }
    }

    object HConfigurationEffectInterpreter {
      // The functor is required for the interpreter.
      implicit val HConfigurationEffectInterpreterFunctor: Functor[HConfigurationEffectInterpreter] =
        new Functor[HConfigurationEffectInterpreter] {
          def map[A, B](fa: HConfigurationEffectInterpreter[A])(f: A => B) =
            fa map f
        }

      // Used internally (see below).
      private def lift[A](x: HConfigurationInterpreter[A]): HConfigurationEffectInterpreter[A] =
        HConfigurationEffectInterpreter(x hom (new (HConfiguration ~> HConfigurationEffect) {
          def apply[X](c: HConfiguration[X]) =
            HConfigurationNoEffect(HConfigurationInterpreter(Suspend(c map (Return(_)))))
        }))

      // Interpreter instruction to set a key/value on the hadoop configuration.
      def set[A](k: String, v: String): HConfigurationEffectInterpreter[Unit] =
        lift(HConfigurationInterpreter.set(k, v))

      // Interpreter instruction to get key's value from the hadoop configuration.
      def get[A](k: String): HConfigurationEffectInterpreter[Option[String]] =
        lift(HConfigurationInterpreter.get(k))

      // Interpreter instruction to unset a key on the hadoop configuration.
      def unset[A](k: String): HConfigurationEffectInterpreter[Unit] =
        lift(HConfigurationInterpreter.unset(k))

      // Interpreter instruction to print (out) the given string.
      def outprintln[A](s: String): HConfigurationEffectInterpreter[Unit] =
        HConfigurationEffectInterpreter(Suspend(HConfigurationOutPrintlnEffect(s, Return(()))))

      // Interpreter instruction to print (err) the given string.
      def errprintln[A](s: String): HConfigurationEffectInterpreter[Unit] =
        HConfigurationEffectInterpreter(Suspend(HConfigurationErrPrintlnEffect(s, Return(()))))

    }

    import HConfigurationEffectInterpreter._

    // A top-level program.
    def program = {
      // A top-level pure-functional program.
      val p =
        for {
          a <- get("a")
          _ <- function1
          _ <- outprintln("a: " + a)
          aa <- get("a")
          _ <- outprintln("a: " + aa)
          b <- get("b")
          _ <- outprintln("b: " + b)
          _ <- function2
          _ <- outprintln("a: " + a)
          aaa <- get("a")
          _ <- outprintln("a: " + aaa)
          bb <- get("b")
          _ <- outprintln("b: " + bb)
          _ <- function3
          _ <- outprintln("a: " + a)
          aaaa <- get("a")
          _ <- errprintln("a: " + aaaa)
          bbb <- get("b")
          _ <- outprintln("b: " + bbb)
        } yield ()
      val conf = setupConfiguration
      // Invoke the unsafe operation (and then forget the configuration ever existed).
      p run conf
    }

    // A pure-functional sub-program.
    def function1 =
      for {
        a <- get("a")
        _ <- outprintln("a: " + a)
        b <- get("b")
        _ <- errprintln("b: " + b)
        _ <- set ("a", "ax")
      } yield ()

    // A pure-functional sub-program.
    def function2 =
      for {
        a <- get("a")
        _ <- outprintln("a: " + a)
        _ <- set ("b", "bx")
        _ <- set ("a", "axx")
      } yield ()

    // A pure-functional sub-program.
    def function3 =
      for {
        _ <- unset("a")
        a <- get("a")
        _ <- outprintln("a: " + a)
        _ <- unset("b")
        _ <- set ("a", "axxx")
      } yield ()
  }

  // Call the `Before` and `After` programs.
  def main(args: Array[String]) {
    Before.program
    println("====")
    After.program
  }
}