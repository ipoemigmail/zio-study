package ipoemi.zio.study.overview._07_testing_effect.db

import zio.Task

trait DatabaseLive extends Database {
  def database: Database.Service =
    new Database.Service {
      def lookup(id: UserID): Task[UserProfile] = ???
      def update(id: UserID, profile: UserProfile): Task[Unit] = ???
    }
}

object DatabaseLive extends DatabaseLive
