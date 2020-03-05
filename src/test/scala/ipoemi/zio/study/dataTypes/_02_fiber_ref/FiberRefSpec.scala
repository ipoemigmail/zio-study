package ipoemi.zio.study.dataTypes._02_fiber_ref

import zio._
import zio.test.Assertion._
import zio.test._

object FiberRefSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("FiberRef")(
      testM("FiberRef") {
        val z = for {
          fiberRef <- FiberRef.make[Int](0)
          _ <- fiberRef.set(10)
          v <- fiberRef.get
        } yield v == 10

        assertM(z)(equalTo(true))
      },
      testM("Operations") {
        val z = for {
          correlationId <- FiberRef.make[String]("")
          v1 <- correlationId.locally("my-correlation-id")(correlationId.get)
          v2 <- correlationId.get
        } yield v1 == "my-correlation-id" && v2 == ""

        assertM(z)(equalTo(true))
      },
      suite("Propagation")(
        testM("1") {
          val z = for {
            fiberRef <- FiberRef.make[Int](0)
            _ <- fiberRef.set(10)
            child <- fiberRef.get.fork
            v <- child.join
          } yield v == 10

          assertM(z)(equalTo(true))
        },
        testM("2") {
          val z = for {
            fiberRef <- FiberRef.make[Int](0)
            latch <- Promise.make[Nothing, Unit]
            fiber <- (fiberRef.set(10) *> latch.succeed(())).fork
            _ <- latch.await
            _ <- fiber.inheritRefs
            v <- fiberRef.get
          } yield v == 10

          assertM(z)(equalTo(true))
        },
        testM("3") {
          val withJoin =
            for {
              fiberRef <- FiberRef.make[Int](0)
              fiber <- fiberRef.set(10).fork
              _ <- fiber.join
              v <- fiberRef.get
            } yield v == 10

          val withoutJoin =
            for {
              fiberRef <- FiberRef.make[Int](0)
              _ <- fiberRef.set(10)
              v <- fiberRef.get
            } yield v == 10

          assertM(withJoin.zip(withoutJoin))(equalTo((true, true)))
        },
        testM("4") {
          val z = for {
            fiberRef <- FiberRef.make(0, math.max)
            child <- fiberRef.update(_ + 1).fork
            _ <- fiberRef.update(_ + 2)
            _ <- child.join
            value <- fiberRef.get
          } yield value == 2

          assertM(z)(equalTo(true))
        }
      )
    )
}
