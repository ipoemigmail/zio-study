package ipoemi.zio.study.overview._07_testing_effect.db
import zio.{Has, Task}

object TestDatabase {
  def live(data: Map[UserID, UserProfile] = Map()): Database = Has(
    new Database.Service {
      private var map: Map[UserID, UserProfile] = data

      def lookup(id: UserID): Task[UserProfile] =
        Task(map(id))

      def update(id: UserID, profile: UserProfile): Task[Unit] =
        Task.effect { map = map + (id -> profile) }
    }
  )
}
