package ipoemi.zio.study.dataTypes._15_tRef

import zio._
import zio.stm.{STM, TQueue, TRef}
import zio.test.Assertion._
import zio.test._

object TRefSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("TRef")(
      suite("Create a TRef")(
        testM("STM") {
          val createTRef: STM[Nothing, TRef[Int]] = TRef.make(10)

          assertM(ZIO(createTRef))(equalTo(createTRef))
        },
        testM("Ref") {
          val commitTRef: UIO[TRef[Int]] = TRef.makeCommit(10)

          assertM(commitTRef.flatMap(_.get.commit))(equalTo(10))
        }
      ),
      suite("Retrieve the value out of a TRef")(
        testM("single transaction") {
          val retrieveSingle: UIO[Int] = (for {
            tRef <- TRef.make(10)
            value <- tRef.get
          } yield value).commit

          assertM(retrieveSingle)(equalTo(10))
        },
        testM("multi transaction") {
          val retrieveMultiple: UIO[Int] = for {
            tRef <- TRef.makeCommit(10)
            value <- tRef.get.commit
          } yield value

          assertM(retrieveMultiple)(equalTo(10))
        }
      ),
      suite("Set a value to a TRef")(
        testM("single transaction") {
          val setSingle: UIO[Int] = (for {
            tRef <- TRef.make(10)
            _ <- tRef.set(20)
            nValue <- tRef.get
          } yield nValue).commit

          assertM(setSingle)(equalTo(20))
        },
        testM("multi transaction") {
          val setMultiple: UIO[Int] = for {
            tRef <- TRef.makeCommit(10)
            nValue <- tRef.set(20).flatMap(_ => tRef.get).commit
          } yield nValue

          assertM(setMultiple)(equalTo(20))
        }
      ),
      suite("Update the value of the TRef")(
        testM("single transaction") {
          val updateSingle: UIO[Int] = (for {
            tRef <- TRef.make(10)
            nValue <- tRef.updateAndGet(_ + 20)
          } yield nValue).commit

          assertM(updateSingle)(equalTo(30))
        },
        testM("multi transaction") {
          val updateMultiple: UIO[Int] = for {
            tRef <- TRef.makeCommit(10)
            nValue <- tRef.updateAndGet(_ + 20).commit
          } yield nValue

          assertM(updateMultiple)(equalTo(30))
        }
      ),
      suite("Modify the value of the TRef")(
        testM("single transaction") {
          val modifySingle: UIO[(String, Int)] = (for {
            tRef <- TRef.make(10)
            mValue <- tRef.modify(v => ("Zee-Oh", v + 10))
            nValue <- tRef.get
          } yield (mValue, nValue)).commit

          assertM(modifySingle)(equalTo(("Zee-Oh", 20)))
        },
        testM("multi transaction") {
          val modifyMultiple: UIO[(String, Int)] = for {
            tRef <- TRef.makeCommit(10)
            tuple2 <- tRef.modify(v => ("Zee-Oh", v + 10)).zip(tRef.get).commit
          } yield tuple2

          assertM(modifyMultiple)(equalTo(("Zee-Oh", 20)))
        }
      ),
      testM("Usage") {
        def transfer(tSender: TRef[Int], tReceiver: TRef[Int], amount: Int): UIO[Int] = {
          STM.atomically {
            for {
              _ <- tSender.get.retryUntil(_ >= amount)
              _ <- tSender.update(_ - amount)
              nAmount <- tReceiver.updateAndGet(_ + amount)
            } yield nAmount
          }
        }

        val transferredMoney: UIO[String] = for {
          tSender <- TRef.makeCommit(50)
          tReceiver <- TRef.makeCommit(100)
          _ <- transfer(tSender, tReceiver, 50).fork
          _ <- tSender.get.retryUntil(_ == 0).commit
          tuple2 <- tSender.get.zip(tReceiver.get).commit
          (senderBalance, receiverBalance) = tuple2
        } yield s"sender: $senderBalance & receiver: $receiverBalance"

        assertM(transferredMoney)(equalTo("sender: 0 & receiver: 150"))
      }
    )
}
