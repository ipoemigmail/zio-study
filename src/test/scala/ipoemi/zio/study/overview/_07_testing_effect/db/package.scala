package ipoemi.zio.study.overview._07_testing_effect

import zio.{Has, ZIO}

package object db {
  type UserID = Int
  type Database = Has[Database.Service]

  def lookup(id: UserID): ZIO[Database, Throwable, UserProfile] =
    ZIO.accessM(_.get.lookup(id))

  def update(id: UserID, profile: UserProfile): ZIO[Database, Throwable, Unit] =
    ZIO.accessM(_.get.update(id, profile))
}
