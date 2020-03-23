package ipoemi.zio.study.dataTypes._05_promise

import zio._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestClock

object PromiseSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("Promise")(
      suite("Operations")(
        suite("Completing")(
          testM("all") {
            val race = for {
              p <- Promise.make[String, Int]
              _ <- p.succeed(1).fork
              _ <- p.complete(ZIO.succeed(2)).fork
              _ <- p.completeWith(ZIO.succeed(3)).fork
              _ <- p.done(Exit.succeed(4)).fork
              _ <- p.fail("5")
              _ <- p.halt(Cause.die(new Error("6")))
              _ <- p.die(new Error("7"))
              _ <- p.interrupt.fork
              value <- p.await
            } yield value
            assertM(race)(equalTo(1))
          },
          testM("act of completing1") {
            val ioPromise1 = Promise.make[Exception, String]
            val ioBooleanSucceeded = ioPromise1.flatMap(promise => promise.succeed("I'm done"))

            assertM(ioBooleanSucceeded)(equalTo(true))
          },
          testM("act of completing2") {
            val ioPromise2 = Promise.make[Exception, Nothing]
            val ioBooleanFailed = ioPromise2.flatMap(promise => promise.fail(new Exception("boom")))

            assertM(ioBooleanFailed)(equalTo(true))
          },
          testM("act of completing3") {
            val ioPromise1 = Promise.make[Exception, String]
            val ioBooleanSucceeded =
              ioPromise1.flatMap(promise => promise.succeed("1").flatMap(_ => promise.succeed("2")))

            assertM(ioBooleanSucceeded)(equalTo(false))
          }
        ),
        testM("Awaiting") {
          val ioPromise3: UIO[Promise[Exception, String]] = Promise.make[Exception, String]
          val ioGet: IO[Exception, String] = ioPromise3.flatMap(promise => promise.succeed("OK").fork *> promise.await)

          assertM(ioGet)(equalTo("OK"))
        },
        suite("Polling")(
          testM("1") {
            val ioPromise4 = Promise.make[Exception, String]
            val ioIsItDone = ioPromise4.flatMap(p => p.poll)

            assertM(ioIsItDone)(isNone)
          },
          testM("2") {
            val ioPromise4 = Promise.make[Exception, String]
            val ioIsItDone2 = ioPromise4.flatMap(p => p.poll.get)

            assertM(ioIsItDone2.run)(fails[Unit](anything))
          },
          testM("3") {
            val ioPromise4 = Promise.make[Exception, String]
            val ioIsItDone2: ZIO[Any, Throwable, String] =
              ioPromise4.flatMap(p => p.succeed("OK") *> p.poll.flatMap(_.get))

            assertM(ioIsItDone2)(equalTo("OK"))
          }
        )
      ),
      testM("Example Usage") {
        import zio.console._
        import zio.duration._
        import zio.clock._

        val program = for {
          promise <- Promise.make[Nothing, String]
          sendHelloWorld = (IO.succeed("hello world") <* sleep(1.second)).flatMap(promise.succeed)
          getAndPrint = promise.await.flatMap(putStrLn(_))
          fiberA <- sendHelloWorld.fork
          fiberB <- getAndPrint.fork
          _ <- TestClock.adjust(5.seconds)
          _ <- (fiberA zip fiberB).join
        } yield ()

        assertM(program)(equalTo(()))
      }
    )
}
