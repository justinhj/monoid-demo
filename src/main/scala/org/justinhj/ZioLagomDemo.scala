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
import zio.duration._

/**
  * Uses ZIO, ZIO config, json4s, Sttp to interrogate a Lagom service registry
  * so it can extract information over the API
  */

object ZioLagomDemo {

  val runtime = Runtime.default

  implicit val serialization = _root_.org.json4s.native.Serialization

  type ServicesResponse = List[Service]

  case class Service(
    name: String,
    url: String,
    portName: String
  )

  type GroupIdsResponse = List[String]

  case class RaidBossInstanceMessage(
    bossDefId: String,
    health: Long,
    instanceId: String,
    leaderboard: List[List[Any]],
    created: Long,
    updated: Long,
    groupId: String
  )

  type GroupBossesSinceTimeResponse = List[RaidBossInstanceMessage]

  // Services URL is the service locator

  case class LagomService(servicesUrl: String, raidbossServiceName: String)
  case class AppConfig(lagomService: LagomService)

  // Zio config descriptor
  val appConfigDescriptor = descriptor[AppConfig]

  case class LeaderboardEntry(name: String, score: Long)

  // Get raid bosses created by guild since a time
  // curl http://127.0.0.1:63859/raidbossapi/raidboss/list/YW1/1 | jq
  // Array of this

  val sample = """[
    {
    "bossDefId": "Btype1",
    "health": 790,
    "instanceId": "raidbossinstance-YW1-Btype1-1578282493317",
    "leaderboard": [
      [
        "chris",
        30
      ],
      [
        "jamie",
        60
      ],
      [
        "justin",
        70
      ],
      [
        "neil",
        50
      ]
    ],
    "created": 1578282493877,
    "updated": 1578282511083,
    "groupId": "YW1"
  }
  ]"""

  def main(args: Array[String]): Unit = {

    val defaultStartTime = 1578371052607L
    val startTime =
      if(args.size > 0)
        Try(args(0).toLong).getOrElse(defaultStartTime)
      else
        defaultStartTime

    val typesafeConfig = ConfigFactory.load()
    val configLayer = TypesafeConfig.fromTypesafeConfig(typesafeConfig, appConfigDescriptor)

    // Requests

    // Get services from Lagom
    def getLagomServicesRequest(uri: Uri) = basicRequest
      .get(uri)
      .response(asJson[ServicesResponse])

    // Get all groups
    def getAllGroupIds(uri: Uri) = basicRequest
      .get(uri)
      .response(asJson[GroupIdsResponse])

    // Get bosses for group with UTC millisecond timestamp
    def bossesForGroupSinceTime(uri: Uri) = basicRequest
      .get(uri)
      .response(asJson[GroupBossesSinceTimeResponse])

    def findService(name: String, services: Seq[Service]): Either[String, Service] = {
      services.find(svc => svc.name == name) match {
        case Some(service) => Right(service)
        case None => Left(s"Service $name was not found")
      }
    }

    val getServices: ZIO[Console with SttpClient with Config[AppConfig], Throwable, ServicesResponse] = {
      for (
        config <- config[AppConfig];
        uri <- ZIO.fromTry(Try(uri"${config.lagomService.servicesUrl}"));
        response <- SttpClient.send(getLagomServicesRequest(uri));
        result <- response.body match {
          case Left(err) => ZIO.fail(err)
          case Right(response) => ZIO.succeed(response)
        }
      ) yield result
    }

    // Get all the group IDs in the system
    def getGroupIds(endpoint: String) = {
      for (
        uri <- ZIO.fromTry(Try(uri"$endpoint/raidbossapi/groupids/list"));
        response <- SttpClient.send(getAllGroupIds(uri));
        result <- response.body match {
          case Left(err) => ZIO.fail(err)
          case Right(response) => ZIO.succeed(response)
        }
      ) yield result
    }

    // For the group ID and timestamp in ms (UTC) will return the list of bosses created since that time
    // Requires the endpoint for the service
    def getBossesForGroupSinceTime(groupId: String, since: Long, endpoint: String)
      : ZIO[Console with SttpClient, Throwable, GroupBossesSinceTimeResponse] = {
      for (
        uri <- ZIO.fromTry(Try(uri"$endpoint/raidbossapi/raidboss/list/$groupId/$since"));
        response <- SttpClient.send(bossesForGroupSinceTime(uri));
        result <- response.body match {
          case Left(err) => ZIO.fail(err)
          case Right(response) => ZIO.succeed(response)
        }
      ) yield result
    }

    def displayActiveBosses(groupIds: List[String], since: Long, endpoint: String) = {
      ZIO.foreach(groupIds) {
        groupId =>
          getBossesForGroupSinceTime(groupId, since, endpoint).flatMap {
            rbis =>
              ZIO.foreach(rbis) { rbi =>
                putStrLn(s"Boss instance: ${rbi.instanceId} health ${rbi.health}")
              }
          }
      }
    }

    def monitorActiveBosses(endpoint: String) = for (
      _ <- putStrLn("Updating active bosses");
      groupIds <- getGroupIds(endpoint);
      _ <- displayActiveBosses(groupIds, startTime, endpoint)
    ) yield ()

    // Given the service address (e.g: http://127.0.0.1:63859) we will call raidbossapi/groupids/list
    // to get all the known groups. The service receives this from the read side Cassandra DB

    val program = (for(
      config <- config[AppConfig];
      serviceName = config.lagomService.raidbossServiceName;
      _ <- putStrLn(s"Finding $serviceName...");
      services <- getServices;
      service <- ZIO.fromEither(findService(serviceName, services));
      _ <- putStrLn(s"Found service: ${service.name} at ${service.url}");
      _ <- monitorActiveBosses(service.url).repeat(Schedule.spaced(10.seconds))
    ) yield ())
        .provideCustomLayer(configLayer ++ AsyncHttpClientZioBackend.layer())

    val handleErrors = program.foldM(
      err => putStrLn(s"Program failed: $err"),
      success => putStrLn(s"Program completed: $success"))

    runtime.unsafeRunSync(handleErrors)
  }
}