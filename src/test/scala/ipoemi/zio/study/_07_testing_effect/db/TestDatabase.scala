package ipoemi.zio.study._07_testing_effect.db

trait TestDatabase extends Database {
  val database: TestDatabaseService = new TestDatabaseService
}
object TestDatabase extends TestDatabase
