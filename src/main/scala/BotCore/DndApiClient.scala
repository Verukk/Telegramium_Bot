package BotCore

import cats.effect.Async
import cats.syntax.all._
import io.circe.Decoder
import io.circe.generic.auto._
import sttp.client4._
import sttp.client4.circe._
import sttp.client4.httpclient.cats.HttpClientCatsBackend

object DndApiClient {

  private val BaseUrl = "https://www.dnd5eapi.co/api"

  case class Monster(index: String, name: String)
  case class MonstersResponse(results: List[Monster])

  case class ArmorClassEntry(`type`: String, value: Int)
  case class SpecialAbility(name: String, desc: String)

  case class MonsterDetails(
    name: String,
    hit_points: Int,
    armor_class: List[ArmorClassEntry],
    challenge_rating: Double
  )

  object MonsterDetails {
    val Default: MonsterDetails = MonsterDetails(
      name = "Unknown",
      hit_points = 0,
      armor_class = List.empty,
      challenge_rating = 0.0
    )
  }

  case class MonsterExtended(
    name: String,
    hit_points: Int,
    armor_class: List[ArmorClassEntry],
    challenge_rating: Double,
    size: String,
    `type`: String,
    alignment: String,
    speed: Map[String, String],
    strength: Int,
    dexterity: Int,
    constitution: Int,
    intelligence: Int,
    wisdom: Int,
    charisma: Int,
    xp: Int,
    languages: String,
    special_abilities: List[SpecialAbility]
  )

  object MonsterExtended {
    val Default: MonsterExtended = MonsterExtended(
      name = "impossible",
      hit_points = 15000,
      armor_class = List.empty,
      challenge_rating = 0.0,
      size = "1",
      `type` = "2",
      alignment = "3",
      speed = Map.empty,
      strength = 1,
      dexterity = 1,
      constitution = 1,
      intelligence = 1,
      wisdom = 1,
      charisma = 1,
      xp = 1,
      languages = "1",
      special_abilities = List.empty
    )
  }

  def fetchMonsters[F[_]: Async]: F[List[Monster]] = {
    HttpClientCatsBackend.resource[F]().use { backend =>
      val request = basicRequest
        .get(uri"$BaseUrl/monsters")
        .response(asJson[MonstersResponse])

      for {
        response <- backend.send(request)
        monstersRes <- response.body match {
          case Right(data) => Async[F].pure(data.results)
          case Left(error) => Async[F].raiseError(new RuntimeException(s"Ошибка при запросе монстров: $error"))
        }
      } yield monstersRes
    }
  }

  def getMonsterInfo[F[_]: Async](index: String): F[Option[MonsterExtended]] = {
    for {
      monstersList <- fetchMonsters[F]
      maybeIndex = monstersList.find(_.index.equalsIgnoreCase(index)).map(_.index)
      details <- maybeIndex match {
        case Some(monsterIndex) => getMonsterFullData[F](List(monsterIndex)).map(_.headOption)
        case None               => Async[F].pure(None)
      }
    } yield details
  }

  def fetchMonsterDetails[F[_]: Async](index: String): F[Option[MonsterDetails]] =
    fetchSingle[MonsterDetails, F](s"$BaseUrl/monsters/$index")

  def fetchMonsterUltraDetailes[F[_]: Async](index: String): F[Option[MonsterExtended]] =
    fetchSingle[MonsterExtended, F](s"$BaseUrl/monsters/$index")

  def getMonstersDetails[F[_]: Async](indexes: List[String]): F[List[MonsterDetails]] =
    indexes.traverse { index =>
      fetchMonsterDetails[F](index).map(_.getOrElse(MonsterDetails.Default))
    }

  def getMonsterFullData[F[_]: Async](indexes: List[String]): F[List[MonsterExtended]] =
    indexes.traverse { index =>
      fetchMonsterUltraDetailes[F](index).map(_.getOrElse(MonsterExtended.Default))
    }

  private def fetchSingle[T: Decoder, F[_]: Async](url: String): F[Option[T]] =
    HttpClientCatsBackend.resource[F]().use { backend =>
      val request = basicRequest.get(uri"$url").response(asJson[T])

      for {
        response <- backend.send(request)
        result <- response.body match {
          case Right(data) => Async[F].pure(Some(data))
          case Left(_)     => Async[F].pure(None)
        }
      } yield result
    }
}
