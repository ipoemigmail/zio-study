package ipoemi.zio.study._03_basic_operations

import zio._
import zio.console._

object BasicOperations {
  val rt: DefaultRuntime = new DefaultRuntime {}

  def main(args: Array[String]): Unit = {
    val succeeded: UIO[Int] = ZIO.succeed(21).map(_ * 2)
    rt.unsafeRun(succeeded)
    val failed: IO[Exception, Unit] = IO.fail("Oh no!").mapError(msg => new Exception(msg))
    val sequenced = getStrLn.flatMap(input => putStrLn(s"You entered: $input"))
    val program =
      for {
        _ <- putStrLn("Hello! What is your name?")
        name <- getStrLn
        _ <- putStrLn(s"Hello, ${name}, welcome to ZIO!")
      } yield ()
    val zipped: UIO[(String, Int)] = ZIO.succeed("4").zip(ZIO.succeed(2))
    val zipRight1 = putStrLn("What is your name?").zipRight(getStrLn)
    val zipRight2 = putStrLn("What is your name?") *> getStrLn
  }
}
