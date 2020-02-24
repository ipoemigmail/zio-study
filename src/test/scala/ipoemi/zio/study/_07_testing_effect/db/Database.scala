package ipoemi.zio.study._07_testing_effect.db
import zio.Task

trait Database {
  def database: Database.Service
}

object Database {
  trait Service {
    def lookup(id: UserID): Task[UserProfile]
    def update(id: UserID, profile: UserProfile): Task[Unit]
  }
}
