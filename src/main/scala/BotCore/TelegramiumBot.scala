package BotCore

import BotCore.DndApiClient._
import cats.Parallel
import cats.effect.Async
import telegramium.bots.high.{Api, LongPollBot}
import telegramium.bots.high.implicits._
import cats.syntax.flatMap._
import cats.syntax.functor._
import telegramium.bots._
import scala.util.Random

class TelegramiumBot[F[_]]()(implicit
  bot: Api[F],
  asyncF: Async[F],
  parallel: Parallel[F]
) extends LongPollBot[F](bot) {

  private def formatShortMonster(m: MonsterDetails): String = {
    s"""
       |  🔥 ${m.name.toUpperCase} 🔥
       |  🛡️ Броня: ${m.armor_class.headOption.map(_.value).getOrElse("Повезло)")}
       |  ❤️ Здоровье: ${m.hit_points}
       |  ⚔️ Уровень Угрозы: ${m.challenge_rating}
       |""".stripMargin.trim
  }

  private def formatExtendedMonster(m: MonsterExtended): String = {
    s"""
       | 🔥 ${m.name.toUpperCase} 🔥
       | ---------------------------------
       | 🛡️ Броня: ${m.armor_class.headOption.map(_.value).getOrElse("Повезло)")}
       | ❤️ Здоровье: ${m.hit_points}
       | ⚔️ Уровень Угрозы: ${m.challenge_rating}
       | ---------------------------------
       | 📏 Размер: ${m.size}
       | 🐾 Тип: ${m.`type`}
       | 🎭 Мировоззрение: ${m.alignment}
       | ---------------------------------
       | 🚶 Скорость:
       | ${if (m.speed.isEmpty) "Нет данных"
      else m.speed.map { case (mode, value) => s"$mode: $value" }.mkString("\n ")}
       |---------------------------------
       | 💪 Сила: ${m.strength}
       | 🤸 Ловкость: ${m.dexterity}
       | 🛡️ Телосложение: ${m.constitution}
       | 🧠 Интеллект: ${m.intelligence}
       | 👁️ Мудрость: ${m.wisdom}
       | 💖 Харизма: ${m.charisma}
       | ---------------------------------
       | 🏆 Опыт: ${m.xp}
       | 🗣️ Языки: ${if (m.languages.isEmpty) "Нет данных" else m.languages}
       | ---------------------------------
       | ✨ Особые способности:
       | ${if (m.special_abilities.isEmpty) "=)"
      else m.special_abilities.map(a => s"   - ${a.name}: ${a.desc}").mkString("\n")}
       |""".stripMargin.trim
  }

  override def onMessage(msg: Message): F[Unit] = {
    msg.text.getOrElse(asyncF.unit) match {
      case "/monsters" =>
        sendMessage(
          chatId = ChatIntId(msg.chat.id),
          text = s"""
              |Ну... Попробуем подобрать существо под вашу хотелку. ✅
              |Прошу вас, выберите сложность...
              |""".stripMargin,
          replyMarkup = Some(
            InlineKeyboardMarkup(
              inlineKeyboard = List(
                List(
                  InlineKeyboardButton("Лёгкий путь", callbackData = Some("easy")),
                  InlineKeyboardButton("Средний", callbackData = Some("medium"))
                ),
                List(
                  InlineKeyboardButton("Тяжело-тяжело...", callbackData = Some("hard")),
                  InlineKeyboardButton(s"💀", callbackData = Some("death"))
                )
              )
            )
          )
        ).exec.void

      case "/generatemonsters" =>
        for {
          monsters <- fetchMonsters[F]
          randomMonsters = Random.shuffle(monsters).take(3)
          monstersDetailed <- getMonstersDetails(randomMonsters.map(_.index))
          monstersText = monstersDetailed.map(formatShortMonster).mkString("\n\n")
          textFinal = s"🌟 Вот 3 случайных монстра 🌟\n\n$monstersText"

          - <- sendMessage(
            chatId = ChatIntId(msg.chat.id),
            text = textFinal
          ).exec.void
        } yield ()

      case "/tags" =>
        for {
          monsters <- fetchMonsters[F]
          monstersRandomIndexes = Random.shuffle(monsters).take(50).map(m => s"🌟${m.index}🌟").mkString("\n")

          textFinal = s"🛡️Вот индексы случайных монстров🛡️ \n\n$monstersRandomIndexes" +
            s"\n\nТеперь вы можете попробовать использовать поиск по тегу 🥳"
          _ <- sendMessage(chatId = ChatIntId(msg.chat.id), text = textFinal).exec.void
        } yield ()

      case text: String if text.startsWith("/find") =>
        val monsterIndex = text.substring(5).trim
        if (monsterIndex.isEmpty) {
          sendMessage(
            chatId = ChatIntId(msg.chat.id),
            text = """Забыли указать тэг, мне нужно знать, какого монстра вы хотите рассмотреть.
                     |Воспользуйтесь командой '/tags', чтобы получить список тэгов.
                     |""".stripMargin
          ).exec.void
        } else {
          for {
            monsterInfo <- getMonsterInfo[F](monsterIndex)
            descriptionText = monsterInfo match {
              case Some(m) => formatExtendedMonster(m)
              case None    => """Такого монстра нет! Скорее всего он прячется от вас..."""
            }
            _ <- sendMessage(
              chatId = ChatIntId(msg.chat.id),
              text = s"""🔮 Сегодня вы поближе познакомитесь с ...🔮
                   |
                   |$descriptionText""".stripMargin
            ).exec.void
          } yield ()
        }

      case "/monster" =>
        for {
          monsters <- fetchMonsters[F]
          randomMonsters = Random.shuffle(monsters).take(1)
          monsterExtendedInfo <- getMonsterFullData(randomMonsters.map(_.index))
          monsterText = monsterExtendedInfo.map(formatExtendedMonster).mkString("\n\n")
          textFinal = s"""🔮 Сегодня вы поближе познакомитесь с ...🔮
                   |
                   |$monsterText""".stripMargin
          _ <- sendMessage(chatId = ChatIntId(msg.chat.id), text = textFinal).exec.void
        } yield ()

      case _ =>
        sendMessage(
          chatId = ChatIntId(msg.chat.id),
          text = """
            | Приключения ждут! Кем сегодня будем убивать игроков?
            |
            | 🐉 /generatemonsters - Вы получите сразу 3ех случайных монстров.🐉
            | 💧 /monsters - Откроет вам снова динамичное меню монстриков.💧
            | 📖/monster - Вы просто получите случайного монстра. С возможностью детального рассмотра.📖
            | 🔒/tags - Вы получите список случайных тэгов монстров. По ним можно искать интересующего вас монстра.🔒
            | 🗺️/find [tag] - Вы получите информацию о монстре по тегу.🗺️
            |""".stripMargin,
          replyMarkup = Some(
            InlineKeyboardMarkup(
              inlineKeyboard = List(
                List(
                  InlineKeyboardButton(EmojiDice.toString(), callbackData = Some("dice"))
                )
              )
            )
          )
        ).exec.void
    }
  }

  override def onCallbackQuery(query: CallbackQuery): F[Unit] = {
    def onMsg(message: Option[MaybeInaccessibleMessage])(f: Message => F[Unit]): F[Unit] =
      message.collect { case m: Message => m }.fold(asyncF.unit)(f)

    def rollTheDice(chatId: Long): F[Unit] = {
      sendMessage(ChatIntId(chatId), text = "Инициатива:" + (Random.nextInt(20) + 1).toString).exec.void >>
        answerCallbackQuery(callbackQueryId = query.id).exec.void
    }

    def mainMenu(chatId: Long): F[Unit] = {
      sendMessage(
        ChatIntId(chatId),
        text = """
            |Ну... допустим 🥳
            |Теперь можете попробовать что-нибудь другое:
            | 🐉 /generatemonsters
            | 💧 /monsters
            | 📖 /monster
            | 🔒 /tags
            | 🗺️ /find [tag]
            |""".stripMargin.trim
      ).exec.void >>
        answerCallbackQuery(callbackQueryId = query.id).exec.void
    }

    def rollMonster(chatId: Long, threat: String): F[Unit] = {
      val (minCr, maxCr) = threat.toLowerCase match {
        case "easy"   => (0.0, 5.0)
        case "medium" => (5.0, 10.0)
        case "hard"   => (10.0, 15.0)
        case "death"  => (15.0, 30.0)
        case _        => (0.0, 20.0)
      }
      for {
        monsters <- fetchMonsters[F]
        filteredMonsters = Random.shuffle(monsters).take(50)
        monsterDetails <- getMonstersDetails(filteredMonsters.map(_.index))

        matchingMonster = monsterDetails.find { m =>
          val cr = m.challenge_rating
          cr >= minCr && cr <= maxCr
        }
        _ <- matchingMonster match {
          case Some(monster) =>
            sendMessage(
              ChatIntId(chatId),
              text = s"""
                                                     |   Новый противник прибыл, и это
                                                     |
                                                     |${formatShortMonster(monster)}
                                                     |""".stripMargin.trim,
              replyMarkup = Some(
                InlineKeyboardMarkup(
                  inlineKeyboard = List(
                    List(
                      InlineKeyboardButton("Пффф, плёвое дело...", callbackData = Some("menu"))
                    )
                  )
                )
              )
            ).exec.void >>
              answerCallbackQuery(callbackQueryId = query.id).exec.void
          case None =>
            sendMessage(
              ChatIntId(chatId),
              text = "Убежал ваш противник..."
            ).exec.void >>
              answerCallbackQuery(callbackQueryId = query.id).exec.void
        }
      } yield ()
    }

    query.data
      .map {
        case "dice"   => onMsg(query.message)(m => rollTheDice(m.chat.id))
        case "info"   => onMsg(query.message)(m => rollTheDice(m.chat.id))
        case "easy"   => onMsg(query.message)(m => rollMonster(m.chat.id, "easy"))
        case "medium" => onMsg(query.message)(m => rollMonster(m.chat.id, "medium"))
        case "hard"   => onMsg(query.message)(m => rollMonster(m.chat.id, "hard"))
        case "death"  => onMsg(query.message)(m => rollMonster(m.chat.id, "death"))
        case "menu"   => onMsg(query.message)(m => mainMenu(m.chat.id))
        case x =>
          answerCallbackQuery(
            callbackQueryId = query.id,
            text = Some(s"Ваш выбор $x")
          ).exec.void
      }
      .getOrElse(asyncF.unit)
  }

  override def onInlineQuery(query: InlineQuery): F[Unit] = {
    answerInlineQuery(
      inlineQueryId = query.id,
      results = query.query
        .split(" ")
        .zipWithIndex
        .map { case (word, idx) =>
          InlineQueryResultArticle(
            id = idx.toString,
            title = word,
            inputMessageContent = InputTextMessageContent(messageText = word)
          )
        }
        .toList
    ).exec.void
  }

  override def onChosenInlineResult(inlineResult: ChosenInlineResult): F[Unit] = {
    import io.circe.syntax._
    import telegramium.bots.CirceImplicits._

    asyncF.delay {
      println(inlineResult.asJson.spaces4)
    }
  }

}
