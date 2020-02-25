package ipoemi.zio.study.overview._07_testing_effect

import zio.ZIO

package object db {
  type UserID = Int

  def lookup(id: UserID): ZIO[Database, Throwable, UserProfile] =
    ZIO.accessM(_.database.lookup(id))

  def update(id: UserID, profile: UserProfile): ZIO[Database, Throwable, Unit] =
    ZIO.accessM(_.database.update(id, profile))
}
