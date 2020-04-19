package ipoemi.zio.study.dataTypes._13_tPromise

import zio._
import zio.stm.{STM, TMap, TPromise}
import zio.test.Assertion._
import zio.test._

object TPromiseSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("TPromise")(
      testM("Create a TPromise") {
        val tPromise = TPromise.make[String, Int]

        assertM(ZIO(tPromise))(equalTo(tPromise))
      },
      suite("Complete a TPromise")(
        testM("success") {
          val tPromiseSucceed: UIO[TPromise[String, Int]] = for {
            tPromise <- TPromise.make[String, Int].commit
            _ <- tPromise.succeed(0).commit
          } yield tPromise

          assertM(tPromiseSucceed.flatMap(_.await.commit))(equalTo(0))
        },
        testM("fail") {
          val tPromiseFail: UIO[TPromise[String, Int]] = for {
            tPromise <- TPromise.make[String, Int].commit
            _ <- tPromise.fail("failed").commit
          } yield tPromise

          assertM(tPromiseFail.flatMap(_.await.commit.run))(fails[String](equalTo("failed")))
        },
        suite("done")(
          testM("doneSucceed") {
            val tPromiseDoneSucceed: UIO[TPromise[String, Int]] = for {
              tPromise <- TPromise.make[String, Int].commit
              _ <- tPromise.done(Right(0)).commit
            } yield tPromise

            assertM(tPromiseDoneSucceed.flatMap(_.await.commit))(equalTo(0))
          },
          testM("donefail") {
            val tPromiseDoneFail: UIO[TPromise[String, Int]] = for {
              tPromise <- TPromise.make[String, Int].commit
              _ <- tPromise.done(Left("failed")).commit
            } yield tPromise

            assertM(tPromiseDoneFail.flatMap(_.await.commit).run)(fails[String](equalTo("failed")))
          }
        )
      ),
      suite("Retrieve the value of a TPromise")(
        testM("poll") {
          val tPromiseOptionValue: UIO[Option[STM[String, Int]]] = for {
            tPromise <- TPromise.make[String, Int].commit
            _ <- tPromise.succeed(0).commit
            res <- tPromise.poll.commit
          } yield res

          assertM(tPromiseOptionValue.map(_.isEmpty))(isFalse)
        },
        testM("await") {
          val tPromiseValue: IO[String, Int] = for {
            tPromise <- TPromise.make[String, Int].commit
            _ <- tPromise.succeed(0).commit
            res <- tPromise.await.commit
          } yield res

          assertM(tPromiseValue)(equalTo(0))
        }
      )
    )
}
