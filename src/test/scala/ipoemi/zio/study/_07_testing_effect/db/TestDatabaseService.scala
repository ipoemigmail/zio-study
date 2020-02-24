package ipoemi.zio.study._07_testing_effect.db

import zio.Task

class TestDatabaseService extends Database.Service {
  private var map: Map[UserID, UserProfile] = Map()

  def setTestData(map0: Map[UserID, UserProfile]): Task[Unit] =
    Task { map = map0 }

  def getTestData: Task[Map[UserID, UserProfile]] =
    Task(map)

  def lookup(id: UserID): Task[UserProfile] =
    Task(map(id))

  def update(id: UserID, profile: UserProfile): Task[Unit] =
    Task.effect { map = map + (id -> profile) }
}
