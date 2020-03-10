package ipoemi.zio.study.dataTypes._03_io

import java.io.{File, FileInputStream, IOException}
import java.nio.charset.StandardCharsets

import org.apache.commons.io.FileUtils
import zio._
import zio.test.Assertion._
import zio.test._

object IoSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("IO")(
      testM("Pure Value") {
        val value = IO.succeed("Hello World")

        assertM(value)(equalTo("Hello World"))
      },
      suite("impure code")(
        testM("1") {
          val effectTotalTask: Task[Long] = IO.effectTotal(System.nanoTime())

          assertM(effectTotalTask.zipWith(effectTotalTask)(_ == _))(equalTo(false))
        },
        testM("2") {
          def readFile(name: String): IO[IOException, Array[Byte]] =
            IO.effect(FileUtils.readFileToByteArray(new File(name))).refineToOrDie[IOException]

          assertM(readFile("not_exists_file").run)(fails[IOException](anything))
        },
        testM("3") {
          type Request = Unit
          type Response = Unit
          type HttpException = Exception
          object Http {
            def req(r: Request, callback: IO[HttpException, Response] => Unit): Unit = {
              callback(IO.fail(new Exception("Error")))
            }
          }

          def makeRequest(req: Request) = IO.effectAsync[HttpException, Response](k => Http.req(req, k))

          assertM(makeRequest(()).run)(fails[HttpException](anything))
        }
      ),
      suite("Mapping")(
        testM("1") {
          val mappedValue = IO.succeed(21).map(_ * 2)
          assertM(mappedValue)(equalTo(42))
        },
        testM("2") {
          val mappedError = IO.fail("No no!").mapError(msg => new Exception(msg))
          assertM(mappedError.run)(fails[Exception](anything))
        }
      ),
      suite("Chaining")(
        testM("1") {
          val chainedActionsValue =
            IO.succeed(List(1, 2, 3)).flatMap { list =>
              IO.succeed(list.map(_ + 1))
            }
          assertM(chainedActionsValue)(equalTo(List(2, 3, 4)))
        },
        testM("2") {
          val chainedActionsValueWithForComprehension = for {
            list <- IO.succeed(List(1, 2, 3))
            added <- IO.succeed(list.map(_ + 1))
          } yield added
          assertM(chainedActionsValueWithForComprehension)(equalTo(List(2, 3, 4)))
        }
      ),
      suite("Brackets")(
        testM("1") {
          var testBuffer = collection.mutable.Buffer.empty[String]
          type Handle = Int
          type Data = String

          def openFile(fileName: String): IO[IOException, Handle] =
            IO.succeed(testBuffer.append("opened")) *> IO.succeed(0)

          def closeFile(handle: Handle): UIO[Unit] =
            IO.succeed(testBuffer.append("closed")) *> IO.succeed(())

          def decodeData(handle: Handle): IO[IOException, Data] = IO.succeed("")
          def groupData(data: Data): IO[IOException, Unit] = IO.succeed(())

          val groupedFileData = openFile("data.json").bracket(closeFile(_)) { file =>
            for {
              data <- decodeData(file)
              grouped <- groupData(data)
            } yield grouped
          }
          assertM(groupedFileData *> IO.succeed(testBuffer.toVector))(equalTo(Vector("opened", "closed")))
        },
        testM("2") {
          var i = 0
          val action = Task.effectTotal(i += 1) *> Task.fail(new Throwable("Boom!"))
          val cleanupAction = UIO.effectTotal(i -= 1)
          val composite = action.ensuring(cleanupAction)
          assertM(composite.either *> IO.succeed(i))(equalTo(0))
        },
        testM("3") {
          def closeStream(is: FileInputStream) =
            UIO(is.close())

          // helper method to work around in Java 8
          def readAll(fis: FileInputStream, len: Long) = {
            val content = Array.ofDim[Byte](len.toInt)
            fis.read(content)
            content
          }

          def convertBytes(is: FileInputStream, len: Long) =
            Task.effect(println(new String(readAll(is, len), StandardCharsets.UTF_8))) // Java 8
          //Task.effect(println(new String(is.readAllBytes(), StandardCharsets.UTF_8))) // Java 11+

          // mybracket is just a value. Won't execute anything here until interpreted
          val mybracket = for {
            file <- Task(new File("build.sbt"))
            len = file.length
            string <- Task(new FileInputStream(file)).bracket(closeStream)(convertBytes(_, len))
          } yield string

          // run my bracket
          def run(args: List[String]) = mybracket.orDie.as(0)

          assertM(run(Nil))(equalTo(0))
        }
      )
    )
}
