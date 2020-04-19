package ipoemi.zio.study.dataTypes._12_tMap

import zio._
import zio.stm.{STM, TMap}
import zio.test.Assertion._
import zio.test._

object TMapSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("TMap")(
      suite("Create TMap")(
        testM("empty") {
          val tMap = TMap.empty[String, Int]

          assertM(ZIO(tMap))(equalTo(tMap))
        },
        testM("specified values") {
          val tMap = TMap.make(("a", 1), ("b", 2), ("c", 3))

          assertM(ZIO(tMap))(equalTo(tMap))
        },
        testM("collection of values") {
          val tMap = TMap.fromIterable(List(("a", 1), ("b", 2), ("c", 3)))

          assertM(ZIO(tMap))(equalTo(tMap))
        }
      ),
      suite("Put a key-value pair to a TMap")(
        testM("put") {
          val putElem: UIO[TMap[String, Int]] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2))
            _ <- tMap.put("c", 3)
          } yield tMap).commit
          assertM(putElem.flatMap(_.get("c").commit))(equalTo(Option(3)))
        },
        testM("merge") {
          val mergeElem: UIO[TMap[String, Int]] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            _ <- tMap.merge("c", 4)((x, y) => x * y)
          } yield tMap).commit
          assertM(mergeElem.flatMap(_.get("c").commit))(equalTo(Option(12)))
        }
      ),
      suite("Remove an element from a TMap")(
        testM("delete") {
          val deleteElem = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            _ <- tMap.delete("b")
          } yield tMap).commit
          assertM(deleteElem.flatMap(_.get("b").commit))(equalTo(None))
        },
        testM("removeIf") {
          val removedEvenValues = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3), ("d", 4))
            _ <- tMap.removeIf((_, v) => v % 2 == 0)
          } yield tMap).commit
          assertM(removedEvenValues.flatMap(_.get("b").commit))(equalTo(None))
        },
        testM("retainIf") {
          val removedEvenValues = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3), ("d", 4))
            _ <- tMap.retainIf((_, v) => v % 2 == 0)
          } yield tMap).commit
          assertM(removedEvenValues.flatMap(_.get("b").commit))(equalTo(Option(2)))
        }
      ),
      suite("Retrieve the value from a TMap")(
        testM("get") {
          val elemGet: UIO[Option[Int]] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            elem <- tMap.get("c")
          } yield elem).commit
          assertM(elemGet)(equalTo(Option(3)))
        },
        testM("getOrElse") {
          val elemGetOrElse: UIO[Int] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            elem <- tMap.getOrElse("d", 4)
          } yield elem).commit
          assertM(elemGetOrElse)(equalTo(4))
        }
      ),
      suite("Transform entries of a TMap")(
        testM("transform") {
          val transformTMap: UIO[TMap[String, Int]] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            _ <- tMap.transform((k, v) => k -> v * v)
          } yield tMap).commit
          assertM(transformTMap.flatMap(_.get("c").commit))(equalTo(Option(9)))
        },
        testM("transform(shrink)") {
          val transformTMap: UIO[TMap[String, Int]] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            _ <- tMap.transform((_, v) => "d" -> v)
          } yield tMap).commit

          val value = transformTMap.flatMap(_.get("c").commit).zip(transformTMap.flatMap(_.get("d").commit))
          assertM(value)(equalTo(((None, Some(3)))))
        },
        testM("transformM") {
          val transformMTMap: UIO[TMap[String, Int]] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            _ <- tMap.transformM((k, v) => STM.succeed(k -> v * v))
          } yield tMap).commit

          assertM(transformMTMap.flatMap(_.get("c").commit))(equalTo(Option(9)))
        },
        testM("transformValues") {
          val transformValuesTMap: UIO[TMap[String, Int]] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            _ <- tMap.transformValues(v => v * v)
          } yield tMap).commit

          assertM(transformValuesTMap.flatMap(_.get("c").commit))(equalTo(Option(9)))
        },
        testM("transformValues") {
          val transformValuesMTMap: UIO[TMap[String, Int]] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            _ <- tMap.transformValuesM(v => STM.succeed(v * v))
          } yield tMap).commit

          assertM(transformValuesMTMap.flatMap(_.get("c").commit))(equalTo(Option(9)))
        },
        testM("fold") {
          val foldTMap: UIO[Int] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            sum <- tMap.fold(0) { case (acc, (_, v)) => acc + v }
          } yield sum).commit

          assertM(foldTMap)(equalTo(6))
        },
        testM("foldM") {
          val foldMTMap: UIO[Int] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            sum <- tMap.foldM(0) { case (acc, (_, v)) => STM.succeed(acc + v) }
          } yield sum).commit

          assertM(foldMTMap)(equalTo(6))
        }
      ),
      suite("Perform side-effect for TMap key-value pairs")(
        testM("foreach") {
          val foreachTMap: ZIO[Any, Nothing, TMap[String, Int]] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            _ <- tMap.foreach((k, v) => STM.succeed(println(s"$k -> $v")))
          } yield tMap).commit

          assertM(foreachTMap.flatMap(_.get("c").commit))(equalTo(Option(3)))
        }
      ),
      suite("Check TMap membership")(
        testM("contains") {
          val tMapContainsValue: UIO[Boolean] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            res <- tMap.contains("a")
          } yield res).commit

          assertM(tMapContainsValue)(equalTo(true))
        }
      ),
      suite("Convert TMap to a List")(
        testM("toList") {
          val tMapTuplesList: UIO[List[(String, Int)]] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            list <- tMap.toList
          } yield list).commit

          assertM(tMapTuplesList.map(_.toSet))(equalTo(Set(("a", 1), ("b", 2), ("c", 3))))
        },
        testM("keys") {
          val tMapKeysList: UIO[List[String]] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            list <- tMap.keys
          } yield list).commit

          assertM(tMapKeysList.map(_.toSet))(equalTo(Set("a", "b", "c")))
        },
        testM("values") {
          val tMapValuesList: UIO[List[Int]] = (for {
            tMap <- TMap.make(("a", 1), ("b", 2), ("c", 3))
            list <- tMap.values
          } yield list).commit

          assertM(tMapValuesList.map(_.toSet))(equalTo(Set(1, 2, 3)))
        }
      )
    )
}
