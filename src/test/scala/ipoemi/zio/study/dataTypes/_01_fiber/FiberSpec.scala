package ipoemi.zio.study.dataTypes._01_fiber

import zio.clock.Clock
import zio.duration._
import zio.test.Assertion._
import zio.test.environment.TestClock
import zio.test.{DefaultRunnableSpec, _}
import zio.{IO, ZIO, _}

object FiberSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("Fiber")(
      testM("Basic1") {
        type Analysis = Int
        val data: Int = 0

        def analyzeData(i: Int): RIO[Clock, Analysis] = ZIO.sleep(1.hour) *> ZIO.effect(1 / i)
        def validateData(i: Int): Task[Boolean] = ZIO.succeed(i != 0)

        val analyzed = for {
          fiber1 <- analyzeData(data).fork // IO[E, Analysis]
          fiber2 <- validateData(data).fork // IO[E, Boolean]
          // Do other stuff
          valid <- fiber2.join
          _ <- if (!valid) fiber1.interrupt else IO.unit
          _ <- TestClock.adjust(10.minutes)
          analyzed <- fiber1.await.map(_.toEither).absolve
        } yield analyzed

        assertM(analyzed.run)(fails[Throwable](anything))
      },
      testM("Basic2") {
        assertM(fib(10))(equalTo(89L))
      },
      testM("Error Model") {
        val error: ZIO[Any, Throwable, String] = IO.fail(new RuntimeException("Some Error"))
        val errorEither: ZIO[Any, Nothing, Either[Throwable, String]] = error.either
        assertM(errorEither)(isLeft(anything))
      },
      testM("Error Model2") {
        val unCatchable = Task.effect(1).map(_ => throw new Exception())
        val unCatchable2 = Task.effect(1).flatMap(_ => throw new Exception())
        val unCatchable3 = Task.effectTotal[Int](throw new Exception())
        assertM(ZIO.succeed(()))(equalTo(()))
      },
      testM("Parallelism") {
        def bigCompute(n1: Int, n2: Int) =
          for {
            t <- fib(n1).zipPar(fib(n2))
            (i1, i2) = t
            r <- ZIO.effectTotal(i1 + i2)
          } yield r
        assertM(bigCompute(5, 7))(equalTo(29L))
      },
      testM("Racing") {
        assertM(fib(8) race fib(3))(equalTo(3L))
      }
    )
}
