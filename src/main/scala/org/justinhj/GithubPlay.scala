package org.justinhj

import github4s.Github
import github4s.domain._
//import github4s.utils.{BaseIntegrationSpec, Integration}
//import BaseIntegrationSpec
import zio._
import zio.console.{Console, getStrLn, putStrLn}
import zio.interop.catz._
import zio.interop.catz.implicits._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import github4s.GithubResponses._
import cats._
import cats.data._
import cats.implicits._
import cats.syntax._

/**
  * Uses ZIO, Cats and github4s
  * Grabs gists and PRs everything else is pretty much the same
  */

object GithubPlay {

  implicit val runtime = Runtime.default
  implicit val executor = runtime.platform.executor

  implicit val ec = executor.asEC

  def main(args: Array[String]): Unit = {

    val headerUserAgent: Map[String, String] = Map("user-agent" -> "github4s")

    val accessToken = sys.env.get("GITHUB_PERSONAL_ACCESS_TOKEN")

    val timeout = 10 seconds

    def getPullRequests(owner: String, repo: String) = {
      Github[Task](accessToken = accessToken,
        timeout = Some(timeout)).pullRequests.listPullRequests(
          owner,
          repo,
          List(PRFilterOpen),
          None,
          headerUserAgent).
      flatMap(ZIO.fromEither(_))
    }

    def getGist(id: String) = {
      Github[Task](accessToken = accessToken,
        timeout = Some(timeout)).gists.getGist(id, None, headerUserAgent).
          flatMap(Task.fromEither(_))
    }

    val seq = getGist("d01704336b9640044d37ad8cedb644ea").zip(getPullRequests("47deg", "github4s"))

    val par = getGist("d01704336b9640044d37ad8cedb644ea").zipPar(getPullRequests("47deg", "github4s"))

    val program = (for(
      _ <- putStrLn(s"Hello!");
      start = System.currentTimeMillis();
      (seqgist, seqprs) <- par;
      end1 = System.currentTimeMillis();
      _ <- putStrLn(seqgist.result.files.keys.mkString(", "));
      _ <- putStrLn(seqprs.result.map(pr => pr.title).mkString(", "));
      (pargist, parprs) <- seq;
      end2 = System.currentTimeMillis();
      _ <- putStrLn(pargist.result.files.keys.mkString(", ") + " " + System.currentTimeMillis());
      _ <- putStrLn(parprs.result.map(pr => pr.title).mkString(", "));
      _ <- putStrLn(s"par ${end1 - start} seq ${end2 - end1}")
      ) yield ())

    runtime.unsafeRun(program)
  }
}