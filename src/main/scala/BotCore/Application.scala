package BotCore

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.client.Client
import org.http4s.client.middleware.Logger

import telegramium.bots.high.Api
import telegramium.bots.high.BotApi

object Application extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO].resource
      .use { httpClient =>
        val http = Logger(logBody = false, logHeaders = false)(httpClient)
        implicit val api: Api[IO] = createBotBackend(http)
        val combatEncounterAssistant = new TelegramiumBot()
        combatEncounterAssistant.start().as(ExitCode.Success)
      }

  private def createBotBackend(http: Client[IO]) =
    BotApi(http, baseUrl = s"https://api.telegram.org/bot7533442422:AAEfnlTwii5xVvWzAcz-n08TvfU2nck-j0o")

}
