# CombatEncounterAssistant

Бот, который использует dnd5eapi для генерации монстров из игры, есть возможность поиска по тэгу.
@CombatEncounterAssistant_bot
Бот написан на Telegramium.

## Запуск

1. Для запуска бота необхидо будет запустить его через файл Application.scala.
2. После чего, бот начнёт работать и ему можно будет писать.

Внутри проекта уже лежит токен бота. Предоставляю его вам 😊

## Инструкции по пользованию

В функционал бота входит следующее:

1. При написании ему любого текста, он выдаст меню с командами и краткой справкой. (И возможностью кинуть кубик к20 на инициативу ❤️)
2. /generatemonsters - бот залезет в api, и выдаст вам 3 случайных монстров с базовыми характеристиками. (Хп, Броня, Угроза).
3. /monsters - вам будет предоставлено окно с 4 кнопками, в зависимости от того, какую выберут - бот подберёт монстра который удовлетворит запросу.
	P.S Там идёт поиск по уровню угрозы, чем меньше, тем легче будет монстр и наоборот. Информация - простая.
4. /monster - если просто интересно, а что такое лежит внутри api. Бот подберёт случайного монстра и выдаст по нему ДЕТАЛЬНУЮ информацию.
5. /tags - вам будет предоставлен список с случайными тэгами монстров. По ним можно будет искать интересные экземпляры.
6. /find 'tag' - при написании этой команды и соответсвующего тэга монстра - он найдёт его в api и выдаст детальную информацию. Если тэга не будет, или он будет с ошибкой - об этом сообщиться.