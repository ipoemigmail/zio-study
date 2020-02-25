package ipoemi.zio.study.overview._05_handling_resources

import java.io.{FileNotFoundException, IOException}

import zio.console.Console
import zio.test.Assertion.equalTo
import zio.test.{DefaultRunnableSpec, _}
import zio.test.environment.TestConsole
import zio.{IO, ZIO, _}

object HandlingResourcesSpec
    extends DefaultRunnableSpec(
      suite("Handling Resource")(
        testM("Finalizing") {
          val finalizer = console.putStrLn("Finalizing!!")
          val finalized = for {
            testConsole <- ZIO.environment[TestConsole]
            _ <- IO.fail("Failed!").ensuring(finalizer.provide(testConsole))
          } yield ()
          assertM(finalized.run *> TestConsole.output, equalTo(Vector("Finalizing!!\n")))
        },
        testM("Bracket") {
          def openFile(file: String): IO[IOException, Array[Byte]] = file match {
            case "primary.data" => IO.fail(new FileNotFoundException())
            case _              => IO.effect(file.getBytes()).refineToOrDie[IOException]
          }
          def closeFile(ba: Array[Byte]) = console.putStrLn("Close!")
          val groupedFileData: ZIO[Console, Throwable, Unit] = openFile("data.json").bracket(closeFile) { ab =>
            ZIO.fail(new Exception("Error!"))
          }
          assertM(groupedFileData.run *> TestConsole.output, equalTo(Vector("Close!\n")))
        }
      )
    )
