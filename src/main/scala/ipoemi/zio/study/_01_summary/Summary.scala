package ipoemi.zio.study._01_summary

import zio._

object Summary {
  def main(args: Array[String]): Unit = {
    val _: UIO[Unit] = null: ZIO[Any, Nothing, Unit]
    val _: URIO[Int, Unit] = null: ZIO[Int, Nothing, Unit]
    val _: Task[Unit] = null: ZIO[Any, Throwable, Unit]
    val _: RIO[Int, Unit] = null: ZIO[Int, Nothing, Unit]
    val _: IO[Int, Unit] = null: ZIO[Any, Int, Unit]
  }
}
