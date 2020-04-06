package ipoemi.zio.study.dataTypes._08_sink

import sun.reflect.generics.reflectiveObjects.NotImplementedException
import zio._
import zio.stream._
import zio.test.Assertion._
import zio.test._

object SinkSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("Sink")(
      testM("Sink") {
        val stream = Stream.fromIterable(1 to 1000)

        val sink = Sink.await[Int]

        assertM(stream.run(sink))(equalTo(1))
      },
      suite("Create sinks")(
        testM("await") {
          val stream = Stream.fromIterable(1 to 1000)

          val sink = Sink.await[Int]

          stream.run(sink)

          assertM(stream.run(sink))(equalTo(1))
        },
        testM("collectAll") {
          val stream = Stream.fromIterable(1 to 1000)

          val sink = Sink.collectAll[Int]

          assertM(stream.run(sink))(equalTo((1 to 1000).toList))
        },
        testM("optional") {
          val stream = Stream.fromIterable(List.empty[Int])

          val sink = Sink.identity[Int].optional

          assertM(stream.run(sink))(equalTo(None))
        },
        testM("while") {
          val stream = Stream.fromIterable(1 to 1000)

          val sink = Sink.collectAllWhile[Int](_ <= 100)

          assertM(stream.run(sink))(equalTo((1 to 100).toList))
        },
        testM("ignore") {
          val stream = Stream.fromIterable(1 to 1000)

          val sink = ZSink.ignoreWhile[Int](_ < 2)

          assertM(stream.run(sink))(equalTo(()))
        },
        testM("drain") {
          val stream = Stream.fromIterable(1 to 1000)

          val sink = Sink.drain

          assertM(stream.run(sink))(equalTo(()))
        },
        testM("fail") {
          val stream = Stream.fromIterable(1 to 1000)

          val sink = Sink.fail[Exception](new NotImplementedException)

          assertM(stream.run(sink).run)(fails[Exception](anything))
        },
        testM("foldLeft") {
          val stream = Stream.fromIterable(1 to 1000)

          val sink = Sink.foldLeft[Int, Int](0)(_ + _)

          assertM(stream.run(sink))(equalTo((1 to 1000).sum))
        },
        testM("fold") {
          val stream = Stream.fromIterable(1 to 1000)

          val sink = Sink.fold(0)(_ <= 100)((acc, n: Int) => (acc + n, Chunk.empty))

          assertM(stream.run(sink))(equalTo((1 to 1000).scanLeft(0)((a, n) => a + n).dropWhile(_ <= 100).head))
        },
        testM("fromFunction") {
          val stream = Stream.fromIterable(1 to 1000)

          val sink = Sink.fromFunction[Int, Int](_ * 2).collectAll

          assertM(stream.run(sink))(equalTo((1 to 1000).map(_ * 2).toList))
        },
        testM("pull1") {
          val stream = Stream.fromIterable(1 to 1000)

          val sink = Sink
            .pull1[String, Int, Int, Int](IO.fail("Empty stream, no value to pull")) { init =>
              Sink.foldLeft[Int, Int](init)(_ + _)
            }

          assertM(stream.run(sink))(equalTo((1 to 1000).sum))
        },
        testM("read1") {
          val stream = Stream.fromIterable(4 to 1000)

          val sink = Sink
            .read1[String, Int] {
              case Some(_) => "Stream is not empty but failed condition"
              case None    => "Stream is empty"
            }(_ > 3)
            .collectAll

          assertM(stream.run(sink))(equalTo((4 to 1000).toList))
        }
      ),
      suite("Transforming sinks")(
        testM("filter") {
          val stream = Stream.fromIterable(4 to 1000)

          val sink = Sink.collectAll[Int].filter(_ > 100)

          assertM(stream.run(sink))(equalTo((1 to 1000).filter(_ > 100).toList))
        },
        testM("race") {
          val stream = Stream.fromIterable(4 to 1000)

          val sink = Sink.foldLeft[Int, Int](0)(_ + _).race(Sink.identity[Int])

          assertM(stream.run(sink))(equalTo(4))
        },
        testM("race") {
          val stream = Stream.fromIterable(4 to 1000)

          val sink = Sink.collectAll[String].contramap[Int](_.toString + "id")

          assertM(stream.run(sink))(equalTo((4 to 1000).map(_.toString + "id").toList))
        },
        testM("dimap") {
          val stream = Stream.fromIterable(4 to 1000)

          val sink = Sink.collectAll[String].dimap[Int, List[String]](_.toString + "id")(_.take(10))

          assertM(stream.run(sink))(equalTo((4 to 1000).map(_.toString + "id").toList.take(10)))
        }
      )
    )
}
