package com.nicta.scoobi
package core

import scalaz._, Scalaz._, Free._
import org.apache.hadoop.conf.Configuration

sealed trait HConfiguration[+A] {
  def map[B](f: A => B): HConfiguration[B] =
    this match {
      case Set(k, v, q) => Set(k, v, f(q))
      case Get(k, q) => Get(k, f compose q)
      case Unset(k, q) => Unset(k, f(q))
    }
}
private case class Set[A](k: String, v: String, q: A) extends HConfiguration[A]
private case class Get[A](k: String, q: Option[String] => A) extends HConfiguration[A]
private case class Unset[A](k: String, q: A) extends HConfiguration[A]

object HConfiguration extends HConfigurationInstances

trait HConfigurationInstances {
  implicit val HConfigurationFunctor: Functor[HConfiguration] =
    new Functor[HConfiguration] {
      def map[A, B](fa: HConfiguration[A])(f: A => B) =
        fa map f
    }
}

sealed trait HConfigurationFree[+A] {
  val free: Free[HConfiguration, A]

  def map[B](f: A => B): HConfigurationFree[B] =
    HConfigurationFree(free map f)

  def flatMap[B](f: A => HConfigurationFree[B]): HConfigurationFree[B] =
    HConfigurationFree(free flatMap (f(_).free))

  def resume: HConfigurationFreeResume[A] =
    free.resume.fold(
      HConfigurationFreeResumeCont(_)
    , HConfigurationFreeResumeTerm(_)
    )

  def hom[G[+_]](f: HConfiguration ~> G)(implicit G: Functor[G]): Free[G, A] =
    free mapSuspension f

  def bounce[AA >: A](f: HConfiguration[HConfigurationFree[A]] => HConfigurationFree[AA]): HConfigurationFree[AA] =
    HConfigurationFree(free.bounce(x => f(x map (HConfigurationFree(_))).free))

  def go[AA >: A](f: HConfiguration[HConfigurationFree[AA]] => HConfigurationFree[AA]): AA =
    free.go[AA](x => f(x map (HConfigurationFree(_))).free)

  def goState[B, AA >: A](b: B)(f: (B, HConfiguration[HConfigurationFree[AA]]) => (B, HConfigurationFree[AA])): (B, AA) = {
    val ff: (B, HConfiguration[Free[HConfiguration, AA]]) => (B, Free[HConfiguration, AA]) =
       (b, x) => {
         val (bb, h) = f(b, x map (HConfigurationFree(_)))
         (bb, h.free)
       }

    free.foldRun[B, AA](b)(ff)
  }

  def zip[B](t: HConfigurationFree[B]): HConfigurationFree[(A, B)] =
    HConfigurationFree(free.zipWith(t.free, (a, b: B) => (a, b)))

  def zipWith[B, C](t: HConfigurationFree[B], f: (A, B) => C): HConfigurationFree[C] =
    HConfigurationFree(free.zipWith(t.free, f))

  def zap[G[+_], B](fs: Cofree[G, A => B])(implicit G: Functor[G], d: Zap[HConfiguration, G]): B =
    free zap fs

  def zapWith[G[+_], B, C](bs: Cofree[G, B])(f: (A, B) => C)(implicit G: Functor[G], d: Zap[HConfiguration, G]): C =
     free.zapWith(bs)(f)

  // CAUTION
  // Unsafe operation. Run once only.
  @annotation.tailrec
  final def run(c: Configuration): A =
    resume match {
      case HConfigurationFreeResumeCont(Set(k, v, q)) =>
        HConfigurationFree({
          c set (k, v)
          q
        }) run c
      case HConfigurationFreeResumeCont(Get(k, q)) =>
        HConfigurationFree(q(Option(c get k))) run c
      case HConfigurationFreeResumeCont(Unset(k, q)) =>
        HConfigurationFree({
          c unset k
          q
        }) run c
      case HConfigurationFreeResumeTerm(a) =>
        a
    }

}

object HConfigurationFree extends HConfigurationFreeFunctions {
  private[core] def apply[A](f: Free[HConfiguration, A]): HConfigurationFree[A] =
    new HConfigurationFree[A] {
      val free = f
    }
}

trait HConfigurationFreeFunctions {
  def set[A](k: String, v: String): HConfigurationFree[Unit] =
    HConfigurationFree(Suspend(Set(k, v, Return(()))))

  def get[A](k: String): HConfigurationFree[Option[String]] =
    HConfigurationFree(Suspend(Get(k, Return(_))))

  def unset[A](k: String): HConfigurationFree[Unit] =
    HConfigurationFree(Suspend(Unset(k, Return(()))))
}

sealed trait HConfigurationFreeResume[+A] {
  def map[B](f: A => B): HConfigurationFreeResume[B] =
    this match {
      case HConfigurationFreeResumeCont(x) =>
        HConfigurationFreeResumeCont(x map (_ map f))
      case HConfigurationFreeResumeTerm(a) =>
        HConfigurationFreeResumeTerm(f(a))
    }

  def free: HConfigurationFree[A] =
    HConfigurationFree(this match {
      case HConfigurationFreeResumeCont(x) =>
        Suspend(x)
      case HConfigurationFreeResumeTerm(a) =>
        Return(a)
    })

  def term: Option[A] =
    this match {
      case HConfigurationFreeResumeCont(_) =>
        None
      case HConfigurationFreeResumeTerm(a) =>
        Some(a)
    }

  def termOr[AA >: A](a: => AA): AA =
    term getOrElse a

  def |[AA >: A](a: => AA): AA =
    termOr(a)

}
private case class HConfigurationFreeResumeCont[+A](x: HConfiguration[Free[HConfiguration, A]]) extends HConfigurationFreeResume[A]
private case class HConfigurationFreeResumeTerm[+A](a: A) extends HConfigurationFreeResume[A]