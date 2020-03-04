package ipoemi.zio.study.overview._07_testing_effect.db

import zio.{Has, Task}

object Database {
  trait Service {
    def lookup(id: UserID): Task[UserProfile]
    def update(id: UserID, profile: UserProfile): Task[Unit]
  }

  val live: Database = Has(
    new Database.Service {
      def lookup(id: UserID): Task[UserProfile] = ???
      def update(id: UserID, profile: UserProfile): Task[Unit] = ???
    }
  )
}
