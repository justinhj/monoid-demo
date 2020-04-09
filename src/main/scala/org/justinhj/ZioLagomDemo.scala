package org.justinhj

import zio._
import zio.console.{Console, putStrLn}
import sttp.client.asynchttpclient.zio.AsyncHttpClientZioBackend
import sttp.client._
import sttp.client.json4s._
import zio.config.typesafe._
import zio.config.magnolia.DeriveConfigDescriptor._
import zio.Runtime
import sttp.client.asynchttpclient.zio.SttpClient
import com.typesafe.config.ConfigFactory
import zio.config.Config
import zio.config.config
import sttp.model.Uri
import scala.util.Try

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

  case class AppConfig(servicesUrl: String, raidbossServiceName: String)
  // Zio config
  val appConfigDescriptor = descriptor[AppConfig]

  def main(args: Array[String]): Unit = {

    val typesafeConfig = ConfigFactory.load()
    val configLayer = TypesafeConfig.fromTypesafeConfig(typesafeConfig, appConfigDescriptor)

    def getServicesRequest(uri: Uri) = basicRequest
      .get(uri)
      .response(asJson[ServicesResponse])

    def findService(name: String, services: Seq[Service]): Either[String, Service] = {
      services.find(svc => svc.name == name) match {
        case Some(service) => Right(service)
        case None => Left(s"Service $name was not found")
      }
    }

    val getServices: ZIO[Console with SttpClient with Config[AppConfig], Throwable, ServicesResponse] = {
      for (
        config <- config[AppConfig];
        uri <- ZIO.fromTry(Try(uri"${config.servicesUrl}"));
        response <- SttpClient.send(getServicesRequest(uri));
        result <- response.body match {
          case Left(err) => ZIO.fail(err)
          case Right(response) => ZIO.succeed(response)
        }
      ) yield result
    }

    val program = (for(
      config <- config[AppConfig];
      serviceName = config.raidbossServiceName;
      _ <- putStrLn(s"Finding $serviceName...");
      services <- getServices;
      service <- ZIO.fromEither(findService(serviceName, services));
      _ <- putStrLn(s"Found service: ${service.name} at ${service.url}")
      ) yield (service))
        .provideCustomLayer(configLayer ++ AsyncHttpClientZioBackend.layer())

    val handleErrors = program.foldM(
      err => putStrLn(s"Program failed: $err"),
      success => putStrLn(s"Program completed: $success"))

    runtime.unsafeRunSync(handleErrors)
  }
}