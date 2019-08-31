package com.avalon.avalongame

import cats.effect.Sync
import cats.implicits._

import scala.util.control.NoStackTrace

trait RandomAlg[F[_]] {
  def shuffle[A](l: List[A]): F[List[A]]
  def randomGet[A](l: List[A]): F[A]
}

object RandomAlg {
  case object EmptyListFound extends RuntimeException with NoStackTrace

  def build[F[_]](implicit F: Sync[F]): RandomAlg[F] = new RandomAlg[F] {
    val random = scala.util.Random

    def shuffle[A](l: List[A]): F[List[A]] = F.delay(random.shuffle(l))
    def randomGet[A](l: List[A]): F[A] = shuffle(l).flatMap(s => F.fromOption(s.headOption, EmptyListFound))
  }
}