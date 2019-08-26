package com.avalon.avalongame

import cats.effect.{ConcurrentEffect, ContextShift, Timer}
import cats.implicits._
import cats.temp.par.Par
import fs2.Stream
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import fs2.Stream

import scala.concurrent.ExecutionContext.global

object AvalongameServer {

  def stream[F[_]: ConcurrentEffect : Par](implicit T: Timer[F], C: ContextShift[F]): Stream[F, Nothing] = {
    for {
      client <- BlazeClientBuilder[F](global).stream
      helloWorldAlg = HelloWorld.impl[F]
      jokeAlg = Jokes.impl[F](client)
      roomManager <- Stream.eval(RoomManager.build[F])
      roomIdGen = RoomIdGenerator.build[F]
      eventManager <- Stream.eval(EventManager.build[F](roomManager, roomIdGen))

      // Combine Service Routes into an HttpApp.
      // Can also be done via a Router if you
      // want to extract a segments not checked
      // in the underlying routes.
      httpApp = (
        AvalongameRoutes.helloWorldRoutes[F](helloWorldAlg) <+>
        AvalongameRoutes.jokeRoutes[F](jokeAlg) <+>
        AvalongameRoutes.websocketRoutes[F](jokeAlg) <+>
        AvalongameRoutes.testRoutessss[F](jokeAlg, eventManager)
      ).orNotFound

      // With Middlewares in place
      finalHttpApp = Logger.httpApp(true, true)(httpApp)

      exitCode <- BlazeServerBuilder[F]
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(finalHttpApp)
        .serve
    } yield exitCode
  }.drain
}