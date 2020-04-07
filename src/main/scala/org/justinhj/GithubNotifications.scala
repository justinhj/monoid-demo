package org.justinhj

import zio._
import zio.console.putStrLn
import sttp.client.asynchttpclient.zio.AsyncHttpClientZioBackend
import sttp.client._
import sttp.client.asynchttpclient.WebSocketHandler
import sttp.client.json4s._

object GithubNotifications {

  implicit val runtime = Runtime.default
  //implicit val executor = runtime.platform.executor

  val accessTokenOpt = sys.env.get("GITHUB_PERSONAL_ACCESS_TOKEN")

  if(accessTokenOpt.isEmpty) {
    println("Please set an environment variable with a github access token called GITHUB_PERSONAL_ACCESS_TOKEN")
    System.exit(1)
  }

  val accessToken = accessTokenOpt.get

  implicit val serialization = _root_.org.json4s.native.Serialization

  case class GistResponse(
    gists: List[Gist]
  )

  case class Gist(
    url: String,
    id: String,
    description: String,
    public: Boolean,
    files: Map[String, GistFile]
  )

  case class GistFile(
      content: String
  )

  // Accept: application/vnd.github.v3+json

  def makeRequestURI(endpoint: String) = uri"https://api.github.com/$endpoint"

  println(makeRequestURI("gists"))

  val headers = Map("Authorization" -> accessToken,
    "Accept" -> "makeRequestURI")

  def getGists() = {
    basicRequest.
      get(makeRequestURI("gists")).
      headers(headers)
      //.
      //response(asJson[List[Gist]])
  }

  def getNotificationsSince(since: String) = {
    basicRequest.
      get(makeRequestURI("notifications")).
      headers(headers + ("If-Modified-Since" -> since))
      //.
      //response(asJson[List[Gist]])
  }

  def main(args: Array[String]): Unit = {

    // ISO 8601
    val lastNotifications = "Fri, 06 Mar 2020 22:13:45 GMT"

    //val managedZioBackend = Managed.make(AsyncHttpClientZioBackend())(what => what.close())

    val program = (for(
        implicit0(backend: SttpBackend[Task,Nothing,WebSocketHandler]) <- AsyncHttpClientZioBackend();
        //response <- getNotificationsSince(lastNotifications).send();
        response <- getGists().send();
        _ <- putStrLn(s"We got the notifications");
        _ <- putStrLn(s"Response: $response")
        ) yield ())

    runtime.unsafeRun(program)
  }

}