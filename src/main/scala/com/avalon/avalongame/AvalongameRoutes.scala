package com.avalon.avalongame

import cats.effect.{Concurrent, Sync, Timer}
import cats.implicits._
import com.avalon.avalongame.Jokes.Joke
import com.avalon.avalongame.events.{IncomingEventDecoder, IncomingEvent, OutgoingEvent, OutgoingEventEncoder}
import fs2.Stream
import fs2.concurrent.Queue
import io.circe.Encoder
import io.circe._
import io.circe.parser._
import org.http4s.{EntityEncoder, HttpRoutes}
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame.Text

import scala.concurrent.duration._

object AvalongameRoutes {
//
  def jokeRoutes[F[_]: Sync](J: Jokes[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "joke" =>
        for {
          joke <- J.get
          resp <- Ok(joke)
        } yield resp
    }
  }

  //ping pong messages exist in http4s
  def websocketRoutes[F[_]: Sync : Timer](J: Jokes[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / "avalon" =>
        WebSocketBuilder[F].build(
          Stream.eval(Timer[F].sleep(5 seconds)) >> Stream.eval(J.get.map(j => Text(Encoder[Joke].apply(j).show))),
          _.drain)
    }
  }

  def testRoutessss[F[_]: Concurrent : Timer](J: Jokes[F], eventManager: EventManager[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / "test" =>
        Queue.bounded[F, OutgoingEvent](10).flatMap { q =>
          WebSocketBuilder[F].build(
            q.dequeue.map(event => Text(OutgoingEventEncoder.encoder.apply(event).show)),
            events => {
              val eventsStream: Stream[F, IncomingEvent] = events.evalMap { wsf =>
                for {
                  parsedJson <- Sync[F].fromEither(parse(Text(wsf.data).str))
                  decodedEvent <- Sync[F].fromEither(IncomingEventDecoder.decoder.decodeJson(parsedJson))

                } yield decodedEvent
              }

              Stream.eval(eventManager.interpret(q, eventsStream))
            }
          )
        }
    }
  }

  def helloWorldRoutes[F[_]: Sync](H: HelloWorld[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "hello" / name =>
        for {
          greeting <- H.hello(HelloWorld.Name(name))
          resp <- Ok(greeting)
        } yield resp
    }
  }
}