package ipoemi.zio.study._02_creation_effect

import java.io.IOException
import java.net.{ServerSocket, Socket}

import zio._
import zio.blocking._
import zio.duration._
import zio.test.Assertion._
import zio.test._

import scala.concurrent.Future
import scala.io.{Codec, Source}
import scala.util.Try

object CreatingEffectSpec
    extends DefaultRunnableSpec(
      suite("CreatingEffect")(
        suite("From Success Values")(
          testM("1") {
            assertM(ZIO.succeed(42), equalTo(42))
          },
          testM("2") {
            assertM(Task.succeed(42), equalTo(42))
          },
          testM("3") {
            lazy val bigList = (0 to 1000000).toList
            lazy val bigString = bigList.map(_.toString).mkString("\n")
            assertM(ZIO.effectTotal(bigString), equalTo(bigString))
          }
        ),
        suite("From Failure Values")(
          testM("1") {
            assertM(ZIO.fail("Uh oh!").run, equalTo(Exit.fail("Uh oh!")))
          },
          testM("2") {
            assertM(Task.fail(new Exception("Uh oh!")).refineToOrDie[Exception].run, fails[Exception](anything))
          }
        ),
        suite("From Success Values")(
          testM("Option1") {
            val zoption1 = ZIO.fromOption(Some(2))
            assertM(zoption1, equalTo(2))
          },
          testM("Option21") {
            val zoption2 = ZIO.fromOption(None)
            assertM(zoption2.run, equalTo(Exit.fail(())))
          },
          testM("Option22") {
            val zoption2 = ZIO.fromOption(None)
            assertM(zoption2.mapError(_ => "It wasn't there!").run, equalTo(Exit.fail("It wasn't there!")))
          },
          testM("Either1") {
            assertM(ZIO.fromEither(Right("Success!")), equalTo("Success!"))
          },
          testM("Either2") {
            assertM(ZIO.fromEither(Left("Failed")).run, equalTo(Exit.fail("Failed")))
          },
          testM("Try") {
            assertM(
              ZIO.fromTry(Try(42 / 0)).refineToOrDie[ArithmeticException].run,
              fails[ArithmeticException](anything)
            )
          },
          testM("Function") {
            assertM(ZIO.fromFunction((i: Int) => i * i).provide(1), equalTo(1))
          },
          testM("Future") {
            lazy val future = Future.successful("Hello!")
            assertM(ZIO.fromFuture { implicit ec =>
              future.map(_ => "Goodbye!")
            }, equalTo("Goodbye!"))
          }
        ),
        suite("From Side-Effects")(
          suite("Synchronous Side-Effects")(
            testM("1") {
              assertM(ZIO.effect( /*StdIn.readLine()*/ "1"), equalTo("1"))
            },
            testM("2") {
              assertM(ZIO.effectTotal(println("side effect!")), equalTo(()))
            },
            testM("From Side-Effects") {
              assertM(
                ZIO.effect(throw new IOException("")).refineToOrDie[IOException].run,
                fails[IOException](anything)
              )
            }
          ),
          suite("Asynchronous Side-Effects")(
            testM("From Side-Effects") {
              val login: IO[AuthError, User] =
                IO.effectAsync[AuthError, User] { callback =>
                  Legacy.login(
                    user => callback(IO.succeed(user)),
                    err => callback(IO.fail(err))
                  )
                }
              assertM(login, equalTo(User("1")))
            }
          )
        ),
        suite("Blocking Synchronous Side-Effects")(
          testM("1") {
            val sleeping = effectBlocking(Thread.sleep(Long.MaxValue))
            assertM(sleeping.timeout(Duration.Zero), equalTo(None))
          },
          testM("2") {
            var th: Thread = null

            def accept(l: ServerSocket): ZIO[Blocking, Throwable, Socket] =
              effectBlockingCancelable { th = Thread.currentThread(); l.accept() }(UIO.effectTotal(l.close()))

            class MyServerSocket(port: Int) extends ServerSocket(port) {
              override def close(): Unit = {
                println("close!!!")
                super.close()
              }
            }

            val s = new MyServerSocket(19992)

            assertM(console.putStrLn("start") *> accept(s).timeout(Duration.Zero), equalTo(None))
          },
          testM("3") {
            def download(url: String): ZIO[Any, Throwable, String] =
              ZIO.bracket(Task.effect(Source.fromURL(url)(Codec.UTF8)))(source => ZIO.effectTotal(source.close)) {
                source =>
                  ZIO.succeed(source.mkString)
              }

            assertM(download("https://zio.dev/"), isNonEmptyString)
          },
          testM("4") {
            def download(url: String): ZIO[Any, Throwable, String] =
              ZIO.bracket(Task.effect(Source.fromURL(url)(Codec.UTF8)))(source => ZIO.effectTotal(source.close)) {
                source =>
                  ZIO.succeed(source.mkString)
              }

            def safeDownload(url: String) = zio.blocking.blocking(download(url))

            assertM(safeDownload("https://zio.dev/"), isNonEmptyString)
          }
        )
      )
    )
