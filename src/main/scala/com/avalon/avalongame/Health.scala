package com.avalon.avalongame

import cats.effect.Sync
import io.chrisdavenport.epimetheus.CollectorRegistry
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.metrics.prometheus.PrometheusExportService

object Health {

  def health[F[_]: Sync](cr: CollectorRegistry[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / "metrics" =>
        PrometheusExportService.generateResponse[F](CollectorRegistry.Unsafe.asJava(cr))
    }
  }
}