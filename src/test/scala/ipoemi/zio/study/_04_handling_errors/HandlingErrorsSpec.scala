package ipoemi.zio.study._04_handling_errors

import java.io.{FileNotFoundException, IOException}

import zio.{IO, UIO, ZIO}
import zio.test.Assertion.{equalTo, fails}
import zio.test._

object HandlingErrorsSpec
    extends DefaultRunnableSpec(
      suite("Handling Errors")(
        testM("Either") {
          val zeither: UIO[Either[String, Int]] = IO.fail("Uh oh!").either
          assertM(zeither, equalTo(Left("Uh oh!")))

          def sqrt(io: UIO[Double]): IO[String, Double] =
            ZIO.absolve(
              io.map(
                value =>
                  if (value < 0.0) Left("Value must be >= 0.0")
                  else Right(Math.sqrt(value))
              )
            )
          assertM(sqrt(ZIO.succeed(-0.1)).run, fails[String](equalTo("Value must be >= 0.0")))
        },
        testM("Catching All Errors") {
          def openFile(file: String): IO[IOException, Array[Byte]] = file match {
            case "primary.json" => IO.fail(new FileNotFoundException())
            case _              => IO.effect(file.getBytes()).refineToOrDie[IOException]
          }
          val z: IO[IOException, Array[Byte]] = openFile("primary.json").catchAll(_ => openFile("backup.json"))
          assertM(z, equalTo("backup.json".getBytes))
        },
        testM("Catching Some Errors") {
          def openFile(file: String): IO[IOException, Array[Byte]] = file match {
            case "primary.data" => IO.fail(new FileNotFoundException())
            case _              => IO.effect(file.getBytes()).refineToOrDie[IOException]
          }
          val data: IO[IOException, Array[Byte]] = openFile("primary.data").catchSome {
            case _: FileNotFoundException => openFile("backup.data")
          }
          assertM(data, equalTo("backup.data".getBytes()))
        },
        testM("Fallback") {
          def openFile(file: String): IO[IOException, Array[Byte]] = file match {
            case "primary.data" => IO.fail(new FileNotFoundException())
            case _              => IO.effect(file.getBytes()).refineToOrDie[IOException]
          }
          val primaryOrBackupData = openFile("primary.data").orElse(openFile("backup.data"))
          assertM(primaryOrBackupData, equalTo("backup.data".getBytes()))
        },
        testM("Folding") {
          def openFile(file: String): IO[IOException, Array[Byte]] = file match {
            case "primary.data" => IO.fail(new FileNotFoundException())
            case _              => IO.effect(file.getBytes()).refineToOrDie[IOException]
          }
          lazy val DefaultData: Array[Byte] = Array(0, 0)
          val primaryOrDefaultData: UIO[Array[Byte]] = openFile("primary.data").fold(_ => DefaultData, data => data)
          assertM(primaryOrDefaultData, equalTo(DefaultData))

          val primaryOrSecondaryData =
            openFile("primary.data").foldM(_ => openFile("secondary.data"), data => ZIO.succeed(data))
          assertM(primaryOrSecondaryData, equalTo("secondary.data".getBytes()))
        }
      )
    )
