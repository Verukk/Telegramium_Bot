package BotCore

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import cats.effect.IO
import cats.effect.unsafe.implicits.global

class DndApiClientFetchMonstersSpec extends AnyFunSuite with Matchers {

  test("fetchMonsters must return nonempty list") {
    val monsters = DndApiClient.fetchMonsters[IO].unsafeRunSync()
    monsters should not be empty
  }

  test("fetchMonsters: each Monster must have nonempty index") {
    val monsters = DndApiClient.fetchMonsters[IO].unsafeRunSync()
    all(monsters.map(_.index)) should not be empty
  }
}

class DndApiClientGetMonsterInfoSpec extends AnyFunSuite with Matchers {

  test("getMonsterInfo for existing index (example, zombie) return Some(...).") {
    val maybeMonster = DndApiClient.getMonsterInfo[IO]("zombie").unsafeRunSync()
    maybeMonster shouldBe defined
    maybeMonster.get.name.toLowerCase should include("zombie")
  }

  test("getMonsterInfo for nonexisting index return None") {
    val maybeMonster = DndApiClient.getMonsterInfo[IO]("unknown-xxx").unsafeRunSync()
    maybeMonster shouldBe None
  }
}

class DndApiClientGetMonstersDetailsSpec extends AnyFunSuite with Matchers {

  test("getMonstersDetails with two existing indexes [zombie, bandit].") {
    val detailsList = DndApiClient.getMonstersDetails[IO](List("zombie", "bandit")).unsafeRunSync()

    detailsList should have size 2

    val zombieData = detailsList.head
    zombieData.name.toLowerCase should include("zombie")

    val banditData = detailsList(1)
    banditData.name.toLowerCase should include("bandit")
  }

  test("getMonstersDetails with nonexisting index return Default.") {
    val detailsList = DndApiClient.getMonstersDetails[IO](List("unknown-monster")).unsafeRunSync()

    detailsList should have size 1

    val unknown = detailsList.head
    unknown.name shouldBe "Unknown"
    unknown.hit_points shouldBe 0
  }
}

class DndApiClientGetMonsterFullDataSpec extends AnyFunSuite with Matchers {

  test("getMonsterFullData for list [zombie, bandit].") {
    val extendedList = DndApiClient.getMonsterFullData[IO](List("zombie", "bandit")).unsafeRunSync()
    extendedList should have size 2

    val zombie = extendedList.head
    zombie.name.toLowerCase should include("zombie")

    val bandit = extendedList(1)
    bandit.name.toLowerCase should include("bandit")
  }

  test("getMonsterFullData for nonexisting index -> MonsterExtended.Default") {
    val extendedList = DndApiClient.getMonsterFullData[IO](List("some-weird-monster")).unsafeRunSync()

    extendedList should have size 1

    val unknown = extendedList.head
    unknown.name shouldBe "impossible"
    unknown.hit_points shouldBe 15000
  }
}
