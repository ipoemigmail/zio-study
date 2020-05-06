package ipoemi.zio.study.howTo._01_use_modules_and_layers

import zio._
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.test.Assertion._
import zio.test._

object UseModulesAndLayersSpec extends DefaultRunnableSpec {
  case class UserId(value: Long)

  type DBConnection = Int
  type DBError = Throwable

  case class User(id: UserId, name: String)

  type UserRepo = Has[UserRepo.Service]

  val userRepoService: UserRepo.Service = new UserRepo.Service {
    var m = Map.empty[UserId, User]
    override def getUser(userId: UserId): IO[DBError, Option[User]] = IO(m.get(userId))
    override def createUser(user: User): IO[DBError, Unit] = IO { m = m + (user.id -> user) }
  }

  object UserRepo {
    trait Service {
      def getUser(userId: UserId): IO[DBError, Option[User]]
      def createUser(user: User): IO[DBError, Unit]
    }

    // This simple live version depends only on a DB Connection
    val inMemory: ZLayer[Any, Nothing, UserRepo] = ZLayer.succeed(userRepoService)

    //accessor methods
    def getUser(userId: UserId): ZIO[UserRepo, DBError, Option[User]] =
      ZIO.accessM(_.get.getUser(userId))

    def createUser(user: User): ZIO[UserRepo, DBError, Unit] =
      ZIO.accessM(_.get.createUser(user))
  }

  type Logging = Has[Logging.Service]

  object Logging {
    trait Service {
      def info(s: String): UIO[Unit]
      def error(s: String): UIO[Unit]
    }

    import zio.console.Console
    val consoleLogger: ZLayer[Console, Nothing, Logging] = ZLayer.fromFunction(
      console =>
        new Service {
          def info(s: String): UIO[Unit] = console.get.putStrLn(s"info - $s")
          def error(s: String): UIO[Unit] = console.get.putStrLn(s"error - $s")
        }
    )

    //accessor methods
    def info(s: String): ZIO[Logging, Nothing, Unit] =
      ZIO.accessM(_.get.info(s))

    def error(s: String): ZIO[Logging, Nothing, Unit] =
      ZIO.accessM(_.get.error(s))
  }

  def spec: ZSpec[Environment, Failure] =
    suite("Use modules and layers")(
      suite("Unleash ZIO environment with ZLayer")(
        testM("A simple case for ZIO environment") {
          var m = Map.empty[UserId, User]
          def getUser(userId: UserId): ZIO[DBConnection, Nothing, Option[User]] = UIO(m.get(userId))
          def createUser(user: User): ZIO[DBConnection, Nothing, Unit] = UIO { m = m + (user.id -> user) }

          val user: User = User(UserId(1234), "Chet")

          val created: ZIO[DBConnection, Nothing, Boolean] = for {
            maybeUser <- getUser(user.id)
            res <- maybeUser.fold(createUser(user).as(true))(_ => ZIO.succeed(false))
          } yield res

          val dbConnection: DBConnection = 1
          val runnable: ZIO[Any, Nothing, Boolean] = created.provide(dbConnection)

          assertM(runnable)(equalTo(true))
        },
        testM("Our first ZIO module") {
          type UserRepo = Has[UserRepo.Service]

          val userRepoService: UserRepo.Service = new UserRepo.Service {
            var m = Map.empty[UserId, User]

            override def getUser(userId: UserId): IO[DBError, Option[User]] = IO(m.get(userId))
            override def createUser(user: User): IO[DBError, Unit] = IO { m = m + (user.id -> user) }
          }

          object UserRepo {
            trait Service {
              def getUser(userId: UserId): IO[DBError, Option[User]]
              def createUser(user: User): IO[DBError, Unit]
            }

            val testRepo: ZLayer[Any, Nothing, UserRepo] = ZLayer.succeed(userRepoService)
          }

          type Logger = Has[Logger.Service]

          val loggerService = new Logger.Service {
            override def log(s: String): UIO[Unit] = UIO(println(s))
          }

          object Logger {
            trait Service {
              def log(s: String): UIO[Unit]
            }

            val testLogger: ZLayer[Any, Nothing, Logger] = ZLayer.succeed(loggerService)
          }

          val repo: UserRepo = Has(userRepoService)
          val logger: Logger = Has(loggerService)

          val mix: UserRepo with Logger = repo ++ logger

          assertM(mix.get[Logger.Service].log("1"))(equalTo(()))
        },
        testM("Wiring modules together") {
          val user2: User = User(UserId(123), "Tommy")

          val makeUser: ZIO[Logging with UserRepo, DBError, Unit] = for {
            _ <- Logging.info(s"inserting user") // ZIO[Logging, Nothing, Unit]
            _ <- UserRepo.createUser(user2) // ZIO[UserRepo, DBError, Unit]
            _ <- Logging.info(s"user inserted") // ZIO[Logging, Nothing, Unit]
          } yield ()

          // compose horizontally
          val horizontal: ZLayer[Console, Nothing, Logging with UserRepo] = Logging.consoleLogger ++ UserRepo.inMemory

          // fulfill missing deps, composing vertically
          val fullLayer: Layer[Nothing, Logging with UserRepo] = Console.live >>> horizontal

          assertM(makeUser.provideLayer(fullLayer))(equalTo(()))
        },
        testM("Providing partial environments") {
          val makeUser2: ZIO[Logging with UserRepo with Clock with Random, DBError, Unit] = for {
            uId <- zio.random.nextLong.map(UserId)
            createdAt <- zio.clock.currentDateTime.orDie
            _ <- Logging.info(s"inserting user")
            _ <- UserRepo.createUser(User(uId, "Chet"))
            _ <- Logging.info(s"user inserted, created at $createdAt")
          } yield ()

          // compose horizontally
          val horizontal: ZLayer[Console, Nothing, Logging with UserRepo] = Logging.consoleLogger ++ UserRepo.inMemory

          // fulfill missing deps, composing vertically
          val fullLayer: Layer[Nothing, Logging with UserRepo] = Console.live >>> horizontal

          val zEnvMakeUser: ZIO[ZEnv, DBError, Unit] = makeUser2.provideCustomLayer(fullLayer)

          assertM(zEnvMakeUser)(equalTo(()))
        },
        suite("Updating local dependencies")(
          testM("update") {
            val horizontal: ZLayer[Console, Nothing, Logging with UserRepo] = Logging.consoleLogger ++ UserRepo.inMemory

            /*
             * 버그 - 타입 파라메터 E 때문에 동작하지 않음
            val withPostgresService: ZLayer[Console, Nothing, Logging with UserRepo] =
              horizontal
                .update[UserRepo.Service] { oldRepo =>
                  new UserRepo.Service {
                    override def getUser(userId: UserId): IO[DBError, Option[User]] = oldRepo.getUser(userId)
                    override def createUser(user: User): IO[DBError, Unit] = oldRepo.createUser(user)
                  }
                }
             */
            val withPostgresService: ZLayer[Console, Nothing, Logging with UserRepo] =
              horizontal >>> ZLayer.fromFunctionMany(_.update[UserRepo.Service]({ oldRepo =>
                new UserRepo.Service {
                  override def getUser(userId: UserId): IO[DBError, Option[User]] =
                    UIO(println("withPostgres")) *> oldRepo.getUser(userId)
                  override def createUser(user: User): IO[DBError, Unit] =
                    UIO(println("withPostgres")) *> oldRepo.createUser(user)
                }
              }))

            val fullLayer: Layer[Nothing, Logging with UserRepo] = Console.live >>> withPostgresService

            val user2: User = User(UserId(123), "Tommy")

            val makeUser: ZIO[Logging with UserRepo, DBError, Unit] = for {
              _ <- Logging.info(s"inserting user") // ZIO[Logging, Nothing, Unit]
              _ <- UserRepo.createUser(user2) // ZIO[UserRepo, DBError, Unit]
              _ <- Logging.info(s"user inserted") // ZIO[Logging, Nothing, Unit]
            } yield ()

            assertM(makeUser.provideLayer(fullLayer))(equalTo(()))
          },
          testM("++") {
            val horizontal: ZLayer[Console, Nothing, Logging with UserRepo] = Logging.consoleLogger ++ UserRepo.inMemory

            val dbLayer: Layer[Nothing, UserRepo] = ZLayer.succeed(new UserRepo.Service {
              var m = Map.empty[UserId, User]
              override def getUser(userId: UserId): IO[DBError, Option[User]] =
                UIO(println("in db")) *> IO(m.get(userId))
              override def createUser(user: User): IO[DBError, Unit] = UIO(println("in db")) *> IO {
                m = m + (user.id -> user)
              }
            })

            val updatedHorizontal2 = horizontal ++ dbLayer

            val fullLayer: Layer[Nothing, Logging with UserRepo] = Console.live >>> updatedHorizontal2

            val user2: User = User(UserId(123), "Tommy")

            val makeUser: ZIO[Logging with UserRepo, DBError, Unit] = for {
              _ <- Logging.info(s"inserting user") // ZIO[Logging, Nothing, Unit]
              _ <- UserRepo.createUser(user2) // ZIO[UserRepo, DBError, Unit]
              _ <- Logging.info(s"user inserted") // ZIO[Logging, Nothing, Unit]
            } yield ()

            assertM(makeUser.provideLayer(fullLayer))(equalTo(()))
          },
          testM("Dealing with managed dependencies") {
            //import java.sql.Connection
            class Connection {
              def close(): Unit = ()
            }

            def makeConnection: UIO[Connection] = UIO(new Connection())

            val connectionLayer: Layer[Nothing, Has[Connection]] =
              ZLayer.fromAcquireRelease(makeConnection)(c => UIO(c.close()))

            val postgresLayer: ZLayer[Has[Connection], Nothing, UserRepo] =
              ZLayer.fromFunction { hasC =>
                new UserRepo.Service {
                  var m = Map.empty[UserId, User]
                  override def getUser(userId: UserId): IO[DBError, Option[User]] =
                    UIO(hasC.get[Connection]).flatMap(_ => IO(m.get(userId)))
                  override def createUser(user: User): IO[DBError, Unit] =
                    UIO(hasC.get[Connection]).flatMap(
                      _ =>
                        IO {
                          m = m + (user.id -> user)
                        }
                    )
                }
              }

            val fullRepo: Layer[Nothing, UserRepo] = connectionLayer >>> postgresLayer

            val user2: User = User(UserId(123), "Tommy")

            val makeUser: ZIO[UserRepo, DBError, Unit] = for {
              _ <- UserRepo.createUser(user2) // ZIO[UserRepo, DBError, Unit]
            } yield ()

            assertM(makeUser.provideLayer(fullRepo))(equalTo(()))
          }
        )
      )
    )
}
