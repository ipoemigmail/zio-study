package ipoemi.zio.study._01_summary

import zio._
import zio.test.Assertion._
import zio.test._

object SummarySpec
    extends DefaultRunnableSpec(
      suite("Summary")(
        testM("Alias") {
          implicitly[UIO[Unit] =:= ZIO[Any, Nothing, Unit]]
          implicitly[URIO[Int, Unit] =:= ZIO[Int, Nothing, Unit]]
          implicitly[Task[Unit] =:= ZIO[Any, Throwable, Unit]]
          implicitly[RIO[Int, Unit] =:= ZIO[Int, Throwable, Unit]]
          implicitly[IO[Int, Unit] =:= ZIO[Any, Int, Unit]]
          assertM(ZIO(()), equalTo(()))
        }
      )
    )
