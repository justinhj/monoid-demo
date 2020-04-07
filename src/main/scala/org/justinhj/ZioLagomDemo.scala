package org.justinhj

import zio._
import zio.console.{Console, putStrLn}
import sttp.client.asynchttpclient.zio.AsyncHttpClientZioBackend
import sttp.client._
import sttp.client.json4s._
import zio.config.typesafe._, TypeSafeConfigSource._
import zio.config.magnolia.DeriveConfigDescriptor._
import zio.Runtime
import sttp.client.asynchttpclient.zio.SttpClient

/**
  * Uses ZIO, Cats and github4s
  * Grabs gists and PRs everything else is pretty much the same
  */

object ZioLagomDemo {

  val runtime = Runtime.default

  implicit val serialization = _root_.org.json4s.native.Serialization

  type ServicesResponse = List[Service]

  // Example:
  //   [{
  //   "name": "lagomraidboss-readside",
  //   "url": "http://127.0.0.1:63859",
  //   "portName": null
  // },...]

  case class Service(
    name: String,
    url: String,
    portName: String
  )

  def main(args: Array[String]): Unit = {

    val request = basicRequest
      .get(uri"http://localhost:9008/services")
      .response(asJson[ServicesResponse])

    val getServices: ZIO[Console with SttpClient, Throwable, List[Service]] = {
        SttpClient.send(request).flatMap { response =>
          response.body.fold(
            err => ZIO.fail(new Exception(s"Failed to get services: ${err.getMessage}")),
            success => ZIO.succeed(success)
          )
        }
    }

    val program = (for(
      _ <- putStrLn("Finding lagomraidboss-readside...");
      services <- getServices;
      _ <- putStrLn(services.toString);
      service = services.find(svc =>
        svc.name == "lagomraidboss-readside");
      _ <- putStrLn(s"Found service: ${service}")
      ) yield (service)).provideCustomLayer(AsyncHttpClientZioBackend.layer())

    runtime.unsafeRunSync(program)

  }
}