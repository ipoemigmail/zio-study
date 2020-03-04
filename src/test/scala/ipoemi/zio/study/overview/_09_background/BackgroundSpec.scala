package ipoemi.zio.study.overview._09_background

import zio._
import zio.test.Assertion._
import zio.test.{DefaultRunnableSpec, testM, _}

object BackgroundSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("Background")(
      testM("1") {
        val example1 = PrintLine(
          "Hello, what is your name?",
          ReadLine(name => PrintLine(s"Good to meet you, ${name}", Return(() => ())))
        )
        ZIO(assert(interpret(example1))(equalTo(())))
      },
      testM("2") {
        val example2: Console[String] =
          for {
            _ <- printLine("What's your name?")
            name <- readLine
            _ <- printLine(s"Hello, ${name}, good to meet you!")
          } yield name
        ZIO(assert(interpret(example2))(equalTo("user1")))
      }
    )
}
