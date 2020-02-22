package ipoemi.zio.study._06_basic_concurrency

import zio.duration._
import zio.test.Assertion._
import zio.test.{DefaultRunnableSpec, assertM, _}
import zio._
import zio.test.environment.TestClock

object BasicConcurrencySpec
    extends DefaultRunnableSpec(
      suite("Basic Concurrency")(
        testM("Fibers") {
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

          assertM(
            counterIO.flatMap(counter => fib100Fiber.provide(counter) *> counter.get),
            equalTo(3736710778780434371L)
          )

          val mes = for {
            fiber <- IO.succeed("Hi!").fork
            message <- fiber.join
          } yield message

          assertM(mes, equalTo("Hi!"))

          val awa = for {
            fiber <- IO.succeed("Hi!").fork
            exit <- fiber.await
          } yield exit

          assertM(awa, equalTo(Exit.Success("Hi!")))

          val interrup = for {
            fiber <- IO.succeed("Hi!").forever.fork
            exit <- fiber.interrupt
          } yield exit

          assertM(interrup, failsCause(anything))

          val interrup2 = for {
            fiber <- IO.succeed("Hi!").forever.fork
            _ <- fiber.interrupt.fork // I don't care!
          } yield ()

          assertM(interrup2, equalTo(()))

          val compo = for {
            fiber1 <- IO.succeed("Hi!").fork
            fiber2 <- IO.succeed("Bye!").fork
            fiber = fiber1.zip(fiber2)
            tuple <- fiber.join
          } yield tuple

          assertM(compo, equalTo(("Hi!", "Bye!")))

          val compo2 = for {
            fiber1 <- IO.fail("Uh oh!").fork
            fiber2 <- IO.succeed("Hurray!").fork
            fiber = fiber1.orElse(fiber2)
            tuple <- fiber.join
          } yield tuple

          assertM(compo2, equalTo("Hurray!"))
        },
        testM("Parallelism") {
          assertM(ZIO(1).zip(ZIO(2)), equalTo((1, 2)))
          assertM(ZIO(1).zipPar(ZIO(2)), equalTo((1, 2)))

          assertM(ZIO(1).zipWith(ZIO(2))((a, b) => a + b), equalTo(3))
          assertM(ZIO(1).zipWithPar(ZIO(2))((a, b) => a + b), equalTo(3))

          assertM(ZIO.collectAll((1 to 100).map(ZIO.succeed)), equalTo((1 to 100).toList))
          assertM(ZIO.collectAllPar((1 to 100).map(ZIO.succeed)), equalTo((1 to 100).toList))

          assertM(ZIO.foreach(1 to 100)(ZIO.succeed), equalTo((1 to 100).toList))
          assertM(ZIO.foreachPar(1 to 100)(ZIO.succeed), equalTo((1 to 100).toList))

          assertM(ZIO.reduceAll(ZIO.succeed(0), (1 to 100).map(ZIO.succeed))((a, b) => a + b), equalTo(5050))
          assertM(ZIO.reduceAllPar(ZIO.succeed(0), (1 to 100).map(ZIO.succeed))((a, b) => a + b), equalTo(5050))

          assertM(ZIO.mergeAll((1 to 100).map(ZIO.succeed))(0)((a, b) => a + b), equalTo(5050))
          assertM(ZIO.mergeAllPar((1 to 100).map(ZIO.succeed))(0)((a, b) => a + b), equalTo(5050))
        },
        testM("Racing") {
          val race = for {
            winner <- IO.succeed("Hello").race(IO.succeed("Goodbye"))
          } yield winner

          assertM(race, equalTo("Hello"))

          val race1 = for {
            winner <- IO.fail("Error").race(IO.succeed("Goodbye"))
          } yield winner

          assertM(race1, equalTo("Goodbye"))
        },
        testM("Timeout") {
          assertM(IO.succeed("Hello").timeout(10.seconds), equalTo(Some("Hello")))
          assertM(IO.succeed("Hello").timeout(Duration.Zero), equalTo(None))

          val timeo = for {
            fiber <- (ZIO.sleep(5.second) *> IO.succeed(1)).timeout(2.second).fork
            _ <- TestClock.adjust(2.seconds)
            r <- fiber.join
          } yield r

          assertM(timeo, equalTo(None))
        }
      )
    )
