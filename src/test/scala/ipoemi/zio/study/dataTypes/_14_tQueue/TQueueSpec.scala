package ipoemi.zio.study.dataTypes._14_tQueue

import zio._
import zio.stm.{STM, TPromise, TQueue}
import zio.test.Assertion._
import zio.test._

object TQueueSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("TQueue")(
      suite("Create a TQueue")(
        testM("bounded") {
          val tQueueBounded: STM[Nothing, TQueue[Int]] = TQueue.bounded[Int](5)

          assertM(ZIO(tQueueBounded))(equalTo(tQueueBounded))
        },
        testM("unbounded") {
          val tQueueUnbounded: STM[Nothing, TQueue[Int]] = TQueue.unbounded[Int]

          assertM(ZIO(tQueueUnbounded))(equalTo(tQueueUnbounded))
        }
      ),
      suite("Put element(s) in a TQueue")(
        testM("offer") {
          val tQueueOffer: UIO[TQueue[Int]] = (for {
            tQueue <- TQueue.bounded[Int](3)
            _ <- tQueue.offer(1)
          } yield tQueue).commit

          assertM(tQueueOffer.flatMap(_.last.commit))(equalTo(1))
        },
        testM("offerAll") {
          val tQueueOfferAll: UIO[TQueue[Int]] = (for {
            tQueue <- TQueue.bounded[Int](3)
            _ <- tQueue.offerAll(List(1, 2))
          } yield tQueue).commit

          assertM(tQueueOfferAll.flatMap(_.last.commit))(equalTo(2))
        }
      ),
      suite("Retrieve element(s) from a TQueue")(
        testM("take") {
          val tQueueTake: UIO[Int] = (for {
            tQueue <- TQueue.bounded[Int](3)
            _ <- tQueue.offerAll(List(1, 2))
            res <- tQueue.take
          } yield res).commit

          assertM(tQueueTake)(equalTo(1))
        },
        testM("poll") {
          val tQueuePoll: UIO[Option[Int]] = (for {
            tQueue <- TQueue.bounded[Int](3)
            res <- tQueue.poll
          } yield res).commit

          assertM(tQueuePoll)(equalTo(None))
        },
        testM("takeUpTo") {
          val tQueueTakeUpTo: UIO[List[Int]] = (for {
            tQueue <- TQueue.bounded[Int](4)
            _ <- tQueue.offerAll(List(1, 2))
            res <- tQueue.takeUpTo(3)
          } yield res).commit

          assertM(tQueueTakeUpTo)(equalTo(List(1, 2)))
        },
        testM("takeAll") {
          val tQueueTakeAll: UIO[List[Int]] = (for {
            tQueue <- TQueue.bounded[Int](4)
            _ <- tQueue.offerAll(List(1, 2))
            res <- tQueue.takeAll
          } yield res).commit

          assertM(tQueueTakeAll)(equalTo(List(1, 2)))
        }
      ),
      testM("Size of a TQueue") {
        val tQueueSize: UIO[Int] = (for {
          tQueue <- TQueue.bounded[Int](3)
          _ <- tQueue.offerAll(List(1, 2))
          size <- tQueue.size
        } yield size).commit

        assertM(tQueueSize)(equalTo(2))
      }
    )
}
