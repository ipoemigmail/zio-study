package ipoemi.zio.study.dataTypes._07_semaphore

import java.util.concurrent.TimeUnit

import zio._
import zio.duration._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestClock

object SemaphoreSpec extends DefaultRunnableSpec {
  def task(name: String) =
    for {
      _ <- console.putStrLn(s"start($name)")
      _ <- ZIO.sleep(Duration(2, TimeUnit.SECONDS))
      _ <- console.putStrLn(s"end($name)")
    } yield ()

  def spec: ZSpec[Environment, Failure] =
    suite("Semaphore")(
      suite("Operations")(
        testM("withPermit") {
          val semTask = (n: Int, sem: Semaphore) =>
            for {
              _ <- sem.withPermit(task(s"withPermit - $n"))
            } yield ()

          val semTaskSeq = (sem: Semaphore) => (1 to 3).map(n => semTask(n, sem))

          val program = for {
            sem <- Semaphore.make(permits = 1)
            seq <- ZIO.effectTotal(semTaskSeq(sem))
            _ <- ZIO.collectAllPar(seq)
          } yield ()

          assertM(TestClock.adjust(1.hour) *> program)(equalTo(()))
        },
        testM("withPermits") {
          val semTask = (n: Int, sem: Semaphore) =>
            for {
              _ <- sem.withPermits(2)(task(s"withPermits - $n"))
            } yield ()

          val semTaskSeq = (sem: Semaphore) => (1 to 3).map(n => semTask(n, sem))

          val program = for {
            sem <- Semaphore.make(permits = 5)
            seq <- ZIO.effectTotal(semTaskSeq(sem))
            _ <- ZIO.collectAllPar(seq)
          } yield ()

          assertM(TestClock.adjust(1.hour) *> program)(equalTo(()))
        }
      )
    )
}
