package com.avalon.avalongame

import cats.effect.{Concurrent, Sync, Timer}
import cats.implicits._
import com.avalon.avalongame.events._
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

  def testRoutessss[F[_]: Concurrent : Timer](eventManager: EventManager[F]): HttpRoutes[F] = {
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

              Stream.eval(eventManager.interpret(q, eventsStream)).handleErrorWith { _ =>
                Stream.eval(Concurrent[F].delay(println("Failed to decode incoming event")))
              }
            }
          )
        }
    }
  }
}