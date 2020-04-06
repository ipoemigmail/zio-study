package ipoemi.zio.study.dataTypes._09_stream

import sun.reflect.generics.reflectiveObjects.NotImplementedException
import zio._
import zio.stream._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestConsole

object StreamSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("Stream")(
      suite("Create Stream")(
        testM("parameters") {
          val stream = Stream(1, 2, 3)

          val sink = Sink.collectAll[Int]

          assertM(stream.run(sink))(equalTo(List(1, 2, 3)))
        },
        testM("iterable") {
          val stream = Stream.fromIterable(0 to 100)

          val sink = Sink.collectAll[Int]

          assertM(stream.run(sink))(equalTo((0 to 100).toList))
        }
      ),
      suite("Transforming a Stream")(
        testM("map") {
          val intStream = Stream.fromIterable(0 to 100)
          val stream = intStream.map(_.toString)

          val sink = Sink.collectAll[String]

          assertM(stream.run(sink))(equalTo((0 to 100).map(_.toString).toList))
        }
      ),
      suite("Consuming a Stream")(
        testM("foreach") {
          val result = for {
            _ <- Stream.fromIterable(0 to 100).foreach(i => console.putStrLn(i.toString))
            output <- ZIO.access[TestConsole](_.get.output).flatten
          } yield output
          assertM(result)(equalTo((0 to 100).map(_.toString + "\n").toVector))
        },
        testM("Using a Sink") {
          def streamReduce(total: Int, element: Int) = total + element
          val result: UIO[Int] = Stream(1, 2, 3).run(Sink.foldLeft(0)(streamReduce))

          assertM(result)(equalTo(List(1, 2, 3).sum))
        }
      ),
      suite("Working on several streams")(
        testM("merge") {
          val merged: Stream[Nothing, Int] = Stream(1, 2, 3).merge(Stream(2, 3, 4))
          val sink = Sink.collectAll[Int]

          assertM(merged.run(sink).map(_.sum))(equalTo(List(1, 2, 3, 2, 3, 4).sum))
        },
        testM("zipping") {
          val zippedStream = Stream(1, 2, 3).zip(Stream(2, 3, 4))
          val sink = Sink.collectAll[(Int, Int)]

          assertM(zippedStream.run(sink))(equalTo(List(1 -> 2, 2 -> 3, 3 -> 4)))
        },
        testM("reduce") {
          val zippedStream = Stream(1, 2, 3).zip(Stream(2, 3, 4))
          def tupleStreamReduce(total: Int, element: (Int, Int)) = {
            val (a, b) = element
            total + (a + b)
          }
          val sink = Sink.foldLeft(0)(tupleStreamReduce)

          assertM(zippedStream.run(sink))(equalTo(List(1, 2, 3).sum + List(2, 3, 4).sum))
        }
      )
    )
}
