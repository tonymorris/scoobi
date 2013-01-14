package com.nicta.scoobi
package core

import scalaz._, Scalaz._, Free._

sealed trait HConfiguration[+A] {
  def map[B](f: A => B): HConfiguration[B] =
    this match {
      case Set(k, v, q) => Set(k, v, f compose q)
      case Get(k, q) => Get(k, f compose q)
      case Unset(k, q) => Unset(k, f compose q)
    }
}
private case class Set[A](k: String, v: String, q: Option[String] => A) extends HConfiguration[A]
private case class Get[A](k: String, q: Option[String] => A) extends HConfiguration[A]
private case class Unset[A](k: String, q: Option[String] => A) extends HConfiguration[A]

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

}

object HConfigurationFree extends HConfigurationFreeFunctions {
  private[core] def apply[A](f: Free[HConfiguration, A]): HConfigurationFree[A] =
    new HConfigurationFree[A] {
      val free = f
    }
}

trait HConfigurationFreeFunctions {
  def set[A](k: String, v: String): HConfigurationFree[Option[String]] =
    HConfigurationFree(Suspend(Set(k, v, Return(_))))

  def get[A](k: String): HConfigurationFree[Option[String]] =
    HConfigurationFree(Suspend(Get(k, Return(_))))

  def unset[A](k: String): HConfigurationFree[Option[String]] =
    HConfigurationFree(Suspend(Unset(k, Return(_))))
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