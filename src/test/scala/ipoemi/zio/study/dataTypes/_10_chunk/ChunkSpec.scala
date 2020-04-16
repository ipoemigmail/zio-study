package ipoemi.zio.study.dataTypes._10_chunk

import zio._
import zio.stream._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestConsole

object ChunkSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("Chunk")(
      testM("Create Chunk") {
        val chunk = Chunk(1, 2, 3)

        assertM(ZIO(chunk))(equalTo(Chunk(1, 2, 3)))
      },
      testM("Concatenating Chunk") {
        val chunk = Chunk(1, 2, 3) ++ Chunk(4, 5, 6)

        assertM(ZIO(chunk))(equalTo(Chunk(1, 2, 3, 4, 5, 6)))
      },
      suite("Collecting Chunk")(
        testM("collect1") {
          val collectChunk = Chunk("Hello ZIO", 1.5, "Hello ZIO NIO", 2.0, "Some string", 2.5)
          val chunk = collectChunk.collect { case s: String => s }

          assertM(ZIO(chunk))(equalTo(Chunk("Hello ZIO", "Hello ZIO NIO", "Some string")))
        },
        testM("collect2") {
          val collectChunk = Chunk("Hello ZIO", 1.5, "Hello ZIO NIO", 2.0, "Some string", 2.5)
          val chunk = collectChunk.collect { case d: Double => d }

          assertM(ZIO(chunk))(equalTo(Chunk(1.5, 2.0, 2.5)))
        },
        testM("collectWhile1") {
          val chunk = Chunk("Sarah", "Bob", "Jane").collectWhile { case element if element != "Bob" => element }

          assertM(ZIO(chunk))(equalTo(Chunk("Sarah")))
        },
        testM("collectWhile2") {
          val chunk = Chunk(9, 2, 5, 1, 6).collectWhile { case element if element >= 2 => element }

          assertM(ZIO(chunk))(equalTo(Chunk(9, 2, 5)))
        }
      ),
      testM("Comparing Chunks") {
        assertM(ZIO(Chunk("A", "B") == Chunk("A", "C")))(equalTo(false))
      },
      suite("Converting Chunks")(
        testM("toArray") {
          val array = Chunk(1, 2, 3).toArray

          assertM(ZIO(array))(equalTo(Array(1, 2, 3)))
        },
        testM("toSeq") {
          val seq = Chunk(1, 2, 3).toSeq

          assertM(ZIO(seq))(equalTo(Seq(1, 2, 3)))
        }
      )
    )
}
