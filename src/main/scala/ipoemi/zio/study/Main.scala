package ipoemi.zio.study

import zio._
import zio.console.Console

object Main extends App {
  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = myLogic.fold(_ => 1, _ => 0)

  def myLogic: ZIO[Console, Throwable, Unit] = console.putStrLn("hello world!")
}
