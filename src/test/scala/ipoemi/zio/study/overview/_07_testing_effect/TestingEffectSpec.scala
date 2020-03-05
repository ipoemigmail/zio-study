package ipoemi.zio.study.overview._07_testing_effect
import zio._
import zio.console._
import zio.test.Assertion._
import zio.test.environment.{TestConsole, TestEnvironment}
import zio.test.{DefaultRunnableSpec, assertM, _}
import db.{Database, _}

object TestingEffectSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("Testing Effect")(
      suite("Environment")(
        testM("Primitive") {
          val t = for {
            env <- ZIO.environment[Has[Int]]
            _ <- putStrLn(s"The value of the environment is: ${env.get}")
          } yield env.get

          assertM(t.provideSomeLayer[Console](ZLayer.succeedMany(Has(2))) *> TestConsole.output)(
            equalTo(Vector("The value of the environment is: 2\n"))
          )
        },
        testM("Case Class") {
          final case class Config(server: String, port: Int)

          val configString: ZIO[Has[Config], Nothing, String] =
            for {
              server <- ZIO.access[Has[Config]](_.get.server)
              port <- ZIO.access[Has[Config]](_.get.port)
            } yield s"Server: $server, port: $port"

          val deps = ZLayer.succeedMany(Has(Config("s", 0)))
          assertM(configString.provideLayer(deps))(equalTo("Server: s, port: 0"))
        },
        testM("Trait") {
          trait DatabaseOps {
            def getTableNames: Task[List[String]]
            def getColumnNames(table: String): Task[List[String]]
          }

          val tablesAndColumns: ZIO[Has[DatabaseOps], Throwable, (List[String], List[String])] =
            for {
              tables <- ZIO.accessM[Has[DatabaseOps]](_.get.getTableNames)
              columns <- ZIO.accessM[Has[DatabaseOps]](_.get.getColumnNames("user_table"))
            } yield (tables, columns)

          val testDatabaseOps = new DatabaseOps {
            override def getTableNames: Task[List[String]] = Task.succeed(List("tname"))
            override def getColumnNames(table: String): Task[List[String]] = Task.succeed(List("cname"))
          }

          val deps = ZLayer.succeedMany(Has(testDatabaseOps))
          assertM(tablesAndColumns.provideLayer(deps))(equalTo((List("tname"), List("cname"))))
        },
        testM("Providing Environment") {
          val square: ZIO[Int, Nothing, Int] =
            for {
              env <- ZIO.environment[Int]
            } yield env * env

          val result: UIO[Int] = square.provide(42)

          assertM(result)(equalTo(42 * 42))
        }
      ),
      testM("Environment Effect") {
        val userId = 0

        val lookedupProfile: ZIO[Database, Throwable, UserProfile] =
          for {
            profile <- db.lookup(userId)
          } yield profile

        val deps = ZLayer.succeedMany(TestDatabase.live(Map(userId -> UserProfile(userId, "user0"))))

        assertM(lookedupProfile.provideLayer(deps))(
          equalTo(UserProfile(userId, "user0"))
        )
      }
    )
}
