package ipoemi.zio.study._04_handling_errors

import java.io.{FileNotFoundException, IOException}

import zio._
import zio.clock._

object HandlingErrors {
  def main(args: Array[String]): Unit = {
    val zeither: UIO[Either[String, Int]] = IO.fail("Uh oh!").either
    def sqrt(io: UIO[Double]): IO[String, Double] =
      ZIO.absolve(
        io.map(
          value =>
            if (value < 0.0) Left("Value must be >= 0.0")
            else Right(Math.sqrt(value))
        )
      )

    def openFile(file: String): IO[IOException, Array[Byte]] = ???

    val z: ZIO[Any, IOException, Array[Byte]] = openFile("primary.json").catchAll(_ => openFile("backup.json"))

    val data: IO[IOException, Array[Byte]] = openFile("primary.data").catchSome {
      case _: FileNotFoundException => openFile("backup.data")
    }

    val primaryOrBackupData: IO[IOException, Array[Byte]] = openFile("primary.data").orElse(openFile("backup.data"))

    lazy val DefaultData: Array[Byte] = Array(0, 0)

    val primaryOrDefaultData: UIO[Array[Byte]] = openFile("primary.data").fold(_ => DefaultData, data => data)

    val primaryOrSecondaryData: IO[IOException, Array[Byte]] =
      openFile("primary.data").foldM(_ => openFile("secondary.data"), data => ZIO.succeed(data))

    val retriedOpenFile: ZIO[Clock, IOException, Array[Byte]] =
      openFile("primary.data").retry(Schedule.recurs(5))

    val v: ZIO[Any, Nothing, Array[Byte]] =
      openFile("primary.data").retryOrElse(Schedule.recurs(5), (_, _: Int) => ZIO.succeed(DefaultData))

    val v1: ZIO[Any, Nothing, Any] =
      openFile("primary.data").retryOrElse(Schedule.recurs(5), (_, _: Int) => ZIO.succeed(1))

    val v2: ZIO[Any, Nothing, Either[Int, Array[Byte]]] =
      openFile("primary.data").retryOrElseEither(Schedule.recurs(5), (_, _: Int) => ZIO.succeed(1))
  }
}
