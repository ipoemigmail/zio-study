package ipoemi.zio.study._05_handling_resources

import java.io.IOException
import java.io.File

import zio._

object HandlingResources {
  val runtime: DefaultRuntime = new DefaultRuntime {}

  def main(args: Array[String]): Unit = {
    val finalizer: UIO[Unit] = UIO.effectTotal(println("Finalizing"))
    val finalized: ZIO[Any, String, Unit] = IO.fail("Failed!").ensuring(finalizer)

    //println(runtime.unsafeRun(finalized))

    def openFile(fileName: String): IO[IOException, File] = IO.effect(new File(fileName)).refineToOrDie[IOException]

    def closeFile(file: File): UIO[Unit] = IO.effectTotal(println("closed"))

    val groupedFileData: IO[IOException, Unit] =
      openFile("data.json").bracket(closeFile) { _ =>
        //ZIO.effectTotal(println("done"))
        ZIO.fail(new IOException("error"))
      }

    runtime.unsafeRun(groupedFileData)
  }
}
