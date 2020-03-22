package ipoemi.zio.study.dataTypes._04_managed

import zio._
import zio.test.Assertion._
import zio.test._

object ManagedSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("Managed")(
      suite("Managed")(
        testM("1") {
          def doSomething(queue: Queue[Int]) = IO.unit

          val managedResource = Managed.make(Queue.unbounded[Int])(_.shutdown)
          val usedResource: UIO[Unit] = managedResource.use { queue =>
            doSomething(queue)
          }

          assertM(usedResource)(equalTo(()))
        },
        testM("2") {
          val managedResource = Managed.make(Queue.unbounded[Int])(_.shutdown)
          val usedResource: UIO[Queue[Int]] = managedResource.use { queue =>
            ZIO.effectTotal(queue)
          }

          assertM(usedResource.flatMap(q => q.size).run.map(_.toEither))(isLeft(anything))
        }
      ),
      suite("Creating a Managed")(
        testM("1") {
          def acquire = IO.effect(())
          val managedFromEffect = Managed.fromEffect(acquire)

          assertM(managedFromEffect.use(a => ZIO(a)))(equalTo(()))
        },
        testM("2") {
          val managedFromValue = Managed.succeed(3)

          assertM(managedFromValue.use(a => ZIO.succeed(a)))(equalTo(3))
        }
      ),
      testM("Managed with ZIO environment") {
        val zManagedResource = ZManaged.make(console.putStrLn("acquiring"))(_ => console.putStrLn("releasing"))
        val zUsedResource = zManagedResource.use { _ =>
          console.putStrLn("running")
        }

        assertM(zUsedResource)(equalTo(()))
      },
      testM("Combining Managed") {
        def openFile(path: String): Task[Int] = ZIO(0)
        def closeFile(handle: Int): UIO[Unit] = UIO(())
        val managedQueue = Managed.make(Queue.unbounded[Int])(_.shutdown)
        val managedFile = Managed.make(openFile("data.json"))(closeFile)

        val combined = for {
          queue <- managedQueue
          file <- managedFile
        } yield (queue, file)

        assertM(combined.use(_ => ZIO(())))(equalTo(()))
      }
    )
}
