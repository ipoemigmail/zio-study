package ipoemi.zio.study.overview._03_basic_operations

import zio._
import zio.test.Assertion._
import zio.test._
import zio.console._
import zio.test.environment.TestConsole

object BasicOperationsSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("Basic Operations")(
      suite("Mapping")(
        testM("1") {
          assertM(IO.succeed(21).map(_ * 2))(equalTo(42))
        },
        testM("2") {
          assertM(IO.fail("No no!").mapError(msg => new Exception(msg)).run)(fails[Exception](anything))
        }
      ),
      testM("Chaining") {
        val chain = getStrLn.flatMap(input => putStrLn(s"You entered: $input"))

        assertM(TestConsole.feedLines("123") *> chain *> TestConsole.output)(
          equalTo(Vector(s"You entered: 123\n"))
        )
      },
      testM("For Comprehensions") {
        val program =
          for {
            _ <- putStrLn("Hello! What is your name?")
            name <- getStrLn
            _ <- putStrLn(s"Hello, ${name}, welcome to ZIO!")
          } yield ()

        assertM(TestConsole.feedLines("123") *> program *> TestConsole.output)(
          equalTo(Vector("Hello! What is your name?\n", s"Hello, 123, welcome to ZIO!\n"))
        )
      },
      suite("Zipping")(
        testM("1") {
          val zipped = ZIO.succeed("4").zip(ZIO.succeed(2))
          assertM(zipped)(equalTo(("4", 2)))
        },
        testM("2") {
          val zipRight1 = putStrLn("What is your name?").zipRight(getStrLn)
          assertM(TestConsole.feedLines("123") *> zipRight1)(equalTo("123"))
        },
        testM("3") {
          val zipRight2 = putStrLn("What is your name?") *> getStrLn
          assertM(TestConsole.feedLines("123") *> zipRight2)(equalTo("123"))
        }
      )
    )
}
