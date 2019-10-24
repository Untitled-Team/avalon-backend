package com.avalon.avalongame

import cats.effect.{ConcurrentEffect, ContextShift, Timer}
import cats.implicits._
import cats.temp.par.Par
import com.avalon.avalongame.common._
import com.avalon.avalongame.events.EventManager
import com.avalon.avalongame.room._
import fs2.Stream
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import fs2.Stream

import scala.concurrent.ExecutionContext.global

object AvalongameServer {

  def stream[F[_]: ConcurrentEffect : Par](implicit T: Timer[F], C: ContextShift[F]): Stream[F, Nothing] = {
    val randomAlg = RandomAlg.build[F]
    val roomIdGen = RoomIdGenerator.build[F]

    for {
      roomManager <- Stream.eval(RoomManager.build[F](randomAlg, roomIdGen))
      eventManager <- Stream.eval(EventManager.build[F](roomManager))

      // Combine Service Routes into an HttpApp.
      // Can also be done via a Router if you
      // want to extract a segments not checked
      // in the underlying routes.
      httpApp = AvalongameRoutes.gameRoutesWS[F](eventManager)
        .orNotFound
//        AvalongameRoutes.helloWorldRoutes[F](helloWorldAlg) <+>
//        AvalongameRoutes.jokeRoutes[F](jokeAlg) <+>
//        AvalongameRoutes.websocketRoutes[F](jokeAlg) <+>


      // With Middlewares in place
      finalHttpApp = Logger.httpApp(true, true)(httpApp)

      exitCode <- BlazeServerBuilder[F]
        .bindHttp(8000, "0.0.0.0")
        .withHttpApp(finalHttpApp)
        .serve
    } yield exitCode
  }.drain
}