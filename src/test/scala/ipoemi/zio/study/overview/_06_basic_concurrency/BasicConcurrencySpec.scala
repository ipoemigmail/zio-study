package ipoemi.zio.study.overview._06_basic_concurrency

import zio.duration._
import zio.test.Assertion._
import zio.test.{DefaultRunnableSpec, assertM, _}
import zio._
import zio.test.environment.TestClock

object BasicConcurrencySpec extends DefaultRunnableSpec {
  def spec =
    suite("Basic Concurrency")(
      suite("Fibers")(
        testM("Forking Effects") {
          val counterIO = Ref.make(0L)

          def fib(n: Long): UIO[Long] = {
            def aux(n: Long, n1: Long, n2: Long): UIO[Long] =
              UIO {
                if (n <= 2) UIO.succeed(n1 + n2)
                else aux(n - 1, n1 + n2, n1)
              }.flatten

            if (n <= 1) UIO.succeed(n)
            else aux(n, 1, 0)
          }

          /*
          def fib(n: Long): UIO[Long] =
            UIO {
              if (n <= 1) UIO.succeed(n)
              else fib(n - 1).zipWith(fib(n - 2))(_ + _)
            }.flatten
           */

          val fib100Fiber: ZIO[Ref[Long], Nothing, Ref[Long]] =
            for {
              counter <- ZIO.environment[Ref[Long]]
              fiber <- fib(100).flatMap(n => counter.update(_ + n)).fork
              _ <- fiber.await
            } yield counter

          assertM(counterIO.flatMap(counter => fib100Fiber.provide(counter) *> counter.get))(
            equalTo(3736710778780434371L)
          )
        },
        testM("Joining Fibers") {
          val mes = for {
            fiber <- IO.succeed("Hi!").fork
            message <- fiber.join
          } yield message

          assertM(mes)(equalTo("Hi!"))
        },
        testM("Awaiting Fibers") {
          val awa = for {
            fiber <- IO.succeed("Hi!").fork
            exit <- fiber.await
          } yield exit

          assertM(awa)(equalTo(Exit.Success("Hi!")))
        },
        suite("Interrupting Fibers")(
          testM("1") {
            val interrup = for {
              fiber <- IO.succeed("Hi!").forever.fork
              exit <- fiber.interrupt
            } yield exit

            assertM(interrup)(failsCause(anything))
          },
          testM("2") {
            val interrup2 = for {
              fiber <- IO.succeed("Hi!").forever.fork
              _ <- fiber.interrupt.fork // I don't care!
            } yield ()

            assertM(interrup2)(equalTo(()))
          }
        ),
        suite("Composing Fibers")(
          testM("1") {
            val compo = for {
              fiber1 <- IO.succeed("Hi!").fork
              fiber2 <- IO.succeed("Bye!").fork
              fiber = fiber1.zip(fiber2)
              tuple <- fiber.join
            } yield tuple

            assertM(compo)(equalTo(("Hi!", "Bye!")))
          },
          testM("2") {
            val compo2 = for {
              fiber1 <- IO.fail("Uh oh!").fork
              fiber2 <- IO.succeed("Hurray!").fork
              fiber = fiber1.orElse(fiber2)
              tuple <- fiber.join
            } yield tuple

            assertM(compo2)(equalTo("Hurray!"))
          }
        )
      ),
      suite("Parallelism")(
        testM("zipPar") {
          assertM(ZIO(1).zipPar(ZIO(2)))(equalTo((1, 2)))
        },
        testM("zipWithPar") {
          assertM(ZIO(1).zipWithPar(ZIO(2))((a, b) => a + b))(equalTo(3))
        },
        testM("collectAllPar") {
          assertM(ZIO.collectAllPar((1 to 100).map(n => ZIO.succeed(n))))(equalTo((1 to 100).toList))
        },
        testM("foreachPar") {
          assertM(ZIO.foreachPar(1 to 100)(n => ZIO.succeed(n)))(equalTo((1 to 100).toList))
        },
        testM("reduceAllPar") {
          assertM(ZIO.reduceAllPar(ZIO.succeed(0), (1 to 100).map(n => ZIO.succeed(n)))((a, b) => a + b))(equalTo(5050))
        },
        testM("mergeAllPar") {
          assertM(ZIO.mergeAllPar((1 to 100).map(n => ZIO.succeed(n)))(0)((a, b) => a + b))(equalTo(5050))
        }
      ),
      suite("Racing")(
        testM("1") {
          val race = for {
            winner <- IO.succeed("Hello").race(IO.succeed("Goodbye"))
          } yield winner

          assertM(race)(equalTo("Hello"))
        },
        testM("2") {
          val race1 = for {
            winner <- IO.fail("Error").race(IO.succeed("Goodbye"))
          } yield winner

          assertM(race1)(equalTo("Goodbye"))
        }
      ),
      suite("Timeout")(
        testM("1") {
          assertM(IO.succeed("Hello").timeout(10.seconds))(equalTo(Some("Hello")))
        },
        testM("2") {
          val timeo = for {
            fiber <- (ZIO.sleep(5.second) *> IO.succeed(1)).timeout(2.second).fork
            _ <- TestClock.adjust(2.seconds)
            r <- fiber.join
          } yield r

          assertM(timeo)(equalTo(None))
        }
      )
    )
}
