package ipoemi.zio.study.dataTypes._11_tArray

import zio._
import zio.stm.{STM, TArray, TRef}
import zio.test.Assertion._
import zio.test._

object TArraySpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("TArray")(
      suite("Create TArray")(
        testM("empty") {
          val tArray = TArray.empty[Int]

          assertM(ZIO(tArray))(equalTo(tArray))
        },
        testM("specified values") {
          val tArray = TArray.make(1, 2, 3)

          assertM(ZIO(tArray))(equalTo(tArray))
        },
        testM("collection of values") {
          val tArray = TArray.fromIterable(List(1, 2, 3))

          assertM(ZIO(tArray))(equalTo(tArray))
        }
      ),
      testM("Retrieve the value from a TArray") {
        val tArrayGetElem: UIO[Int] = (
          for {
            tArray <- TArray.make(1, 2, 3, 4)
            elem <- tArray(2)
          } yield elem
        ).commit

        assertM(tArrayGetElem)(equalTo(3))
      },
      suite("Update the value of a TArray")(
        testM("update") {
          val tArrayUpdateElem: ZIO[Any, Nothing, TArray[Int]] = (
            for {
              tArray <- TArray.make(1, 2, 3, 4)
              _ <- tArray.update(2, el => el + 10)
            } yield tArray
          ).commit
          assertM(tArrayUpdateElem.flatMap(_.apply(2).commit))(equalTo(13))
        },
        testM("updateM") {
          val tArrayUpdateMElem = (for {
            tArray <- TArray.make(1, 2, 3, 4)
            _ <- tArray.updateM(2, el => STM.succeed(el + 10))
          } yield tArray).commit
          assertM(tArrayUpdateMElem.flatMap(_.apply(2).commit))(equalTo(13))
        }
      ),
      suite("Transform elements of a TArray")(
        testM("transform") {
          val transformTArray = (for {
            tArray <- TArray.make(1, 2, 3, 4)
            _ <- tArray.transform(a => a * a)
          } yield tArray).commit
          assertM(transformTArray.flatMap(_.apply(2).commit))(equalTo(9))
        },
        testM("transformM") {
          val transformTArray = (for {
            tArray <- TArray.make(1, 2, 3, 4)
            _ <- tArray.transformM(a => STM.succeed(a * a))
          } yield tArray).commit
          assertM(transformTArray.flatMap(_.apply(2).commit))(equalTo(9))
        },
        testM("fold") {
          val foldTArray: UIO[Int] = (for {
            tArray <- TArray.make(1, 2, 3, 4)
            sum <- tArray.fold(0)(_ + _)
          } yield sum).commit
          assertM(foldTArray)(equalTo(List(1, 2, 3, 4).sum))
        },
        testM("foldM") {
          val foldTArray: UIO[Int] = (for {
            tArray <- TArray.make(1, 2, 3, 4)
            sum <- tArray.foldM(0)((acc, el) => STM.succeed(acc + el))
          } yield sum).commit
          assertM(foldTArray)(equalTo(List(1, 2, 3, 4).sum))
        }
      ),
      suite("Perform side-effect for TArray elements")(
        testM("foreach") {
          val foreachTArray: UIO[TArray[Int]] = (for {
            tArray <- TArray.make(1, 2, 3, 4)
            _ <- tArray.foreach(a => STM.succeed(println(a)))
          } yield tArray).commit
          assertM(foreachTArray.flatMap(_.apply(0).commit))(equalTo(1))
        }
      )
    )
}
