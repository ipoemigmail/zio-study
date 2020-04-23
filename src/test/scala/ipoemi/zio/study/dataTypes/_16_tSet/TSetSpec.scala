package ipoemi.zio.study.dataTypes._16_tSet

import zio._
import zio.stm.{STM, TSet}
import zio.test.Assertion._
import zio.test._

object TSetSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("TSet")(
      suite("Create a TSet")(
        testM("empty") {
          val emptyTSet = TSet.empty[Int]

          assertM(ZIO(emptyTSet))(equalTo(emptyTSet))
        },
        testM("make") {
          val specifiedValuesTSet: STM[Nothing, TSet[Int]] = TSet.make(1, 2, 3)

          assertM(ZIO(specifiedValuesTSet))(equalTo(specifiedValuesTSet))
        },
        testM("fromIterable") {
          val iterableTSet: STM[Nothing, TSet[Int]] = TSet.fromIterable(List(1, 2, 3))

          assertM(ZIO(iterableTSet))(equalTo(iterableTSet))
        }
      ),
      suite("Put an element to a TSet")(
        testM("putElem") {
          val putElem: UIO[TSet[Int]] = (for {
            tSet <- TSet.make(1, 2)
            _ <- tSet.put(3)
          } yield tSet).commit

          assertM(putElem.flatMap(_.contains(3).commit))(equalTo(true))
        }
      ),
      suite("Remove an element from a TSet")(
        testM("delete") {
          val deleteElem: UIO[TSet[Int]] = (for {
            tSet <- TSet.make(1, 2, 3)
            _ <- tSet.delete(1)
          } yield tSet).commit

          assertM(deleteElem.flatMap(_.contains(1).commit))(equalTo(false))
        },
        testM("removeIf") {
          val removedEvenElems: UIO[TSet[Int]] = (for {
            tSet <- TSet.make(1, 2, 3, 4)
            _ <- tSet.removeIf(_ % 2 == 0)
          } yield tSet).commit

          assertM(removedEvenElems.flatMap(_.contains(2).commit))(equalTo(false))
        },
        testM("retainIf") {
          val retainedEvenElems: UIO[TSet[Int]] = (for {
            tSet <- TSet.make(1, 2, 3, 4)
            _ <- tSet.retainIf(_ % 2 == 0)
          } yield tSet).commit
          assertM(retainedEvenElems.flatMap(_.contains(2).commit))(equalTo(true))
        }
      ),
      suite("Union of a TSet")(
        testM("union") {
          val unionTSet: UIO[TSet[Int]] = (for {
            tSetA <- TSet.make(1, 2, 3, 4)
            tSetB <- TSet.make(3, 4, 5, 6)
            _ <- tSetA.union(tSetB)
          } yield tSetA).commit

          assertM(unionTSet.flatMap(_.size.commit))(equalTo(6))
        }
      ),
      suite("Intersection of a TSet")(
        testM("intersection") {
          val intersectionTSet: UIO[TSet[Int]] = (for {
            tSetA <- TSet.make(1, 2, 3, 4)
            tSetB <- TSet.make(3, 4, 5, 6)
            _ <- tSetA.intersect(tSetB)
          } yield tSetA).commit

          assertM(intersectionTSet.flatMap(_.size.commit))(equalTo(2))
        }
      ),
      suite("Difference of a TSet")(
        testM("diff") {
          val diffTSet: UIO[TSet[Int]] = (for {
            tSetA <- TSet.make(1, 2, 3, 4)
            tSetB <- TSet.make(3, 4, 5, 6)
            _ <- tSetA.diff(tSetB)
          } yield tSetA).commit

          assertM(diffTSet.flatMap(_.size.commit))(equalTo(2))
        }
      ),
      suite("Transform elements of a TSet")(
        testM("transform") {
          val transformTSet: UIO[TSet[Int]] = (for {
            tSet <- TSet.make(1, 2, 3, 4)
            _ <- tSet.transform(a => a * a)
          } yield tSet).commit

          assertM(transformTSet.flatMap(_.toList.commit).map(_.sum))(equalTo(30))
        },
        testM("shrink") {
          val shrinkTSet: UIO[TSet[Int]] = (for {
            tSet <- TSet.make(1, 2, 3, 4)
            _ <- tSet.transform(_ => 1)
          } yield tSet).commit

          assertM(shrinkTSet.flatMap(_.size.commit))(equalTo(1))
        },
        testM("transformM") {
          val transformMTSet: UIO[TSet[Int]] = (for {
            tSet <- TSet.make(1, 2, 3, 4)
            _ <- tSet.transformM(a => STM.succeed(a * a))
          } yield tSet).commit

          assertM(transformMTSet.flatMap(_.toList.commit).map(_.sum))(equalTo(30))
        },
        testM("fold") {
          val foldTSet: UIO[Int] = (for {
            tSet <- TSet.make(1, 2, 3, 4)
            sum <- tSet.fold(0)(_ + _)
          } yield sum).commit

          assertM(foldTSet)(equalTo(10))
        },
        testM("foldM") {
          val foldMTSet: UIO[Int] = (for {
            tSet <- TSet.make(1, 2, 3, 4)
            sum <- tSet.foldM(0)((acc, el) => STM.succeed(acc + el))
          } yield sum).commit

          assertM(foldMTSet)(equalTo(10))
        }
      ),
      suite("Perform side-effect for TSet elements")(
        testM("foreach") {
          val foreachTSet: UIO[TSet[Int]] = (for {
            tSet <- TSet.make(1, 2, 3, 4)
            _ <- tSet.foreach(a => STM.succeed(println(a)))
          } yield tSet).commit

          assertM(foreachTSet.flatMap(_.size.commit))(equalTo(4))
        }
      ),
      suite("Check TSet membership")(
        testM("contains") {
          val tSetContainsElem: UIO[Boolean] = (for {
            tSet <- TSet.make(1, 2, 3, 4)
            res <- tSet.contains(3)
          } yield res).commit

          assertM(tSetContainsElem)(equalTo(true))
        }
      ),
      suite("Convert TSet to a List")(
        testM("toList") {
          val tSetToList: UIO[List[Int]] = (for {
            tSet <- TSet.make(1, 2, 3, 4)
            list <- tSet.toList
          } yield list).commit

          assertM(tSetToList.map(_.sum))(equalTo(10))
        }
      ),
      suite("Size of a TSet")(
        testM("size") {
          val tSetSize: UIO[Int] = (for {
            tSet <- TSet.make(1, 2, 3, 4)
            size <- tSet.size
          } yield size).commit

          assertM(tSetSize)(equalTo(4))
        }
      )
    )
}
