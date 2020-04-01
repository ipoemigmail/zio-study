package ipoemi.zio.study.dataTypes._06_schedule

import zio._
import zio.duration.Duration
import zio.test.Assertion._
import zio.test._
import zio.duration._
import zio.test.environment.TestClock

object ScheduleSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("Schedule")(
      suite("Repetition")(
        testM("repeat") {
          val value = IO(1).repeat(Schedule.recurs(1))
          assertM(value)(equalTo(1))
        },
        testM("repeatOrElse") {
          val value = IO.fail(new Exception()).repeatOrElse(Schedule.recurs(2), (_, _: Option[Int]) => ZIO(2))
          assertM(value)(equalTo(2))
        },
        testM("repeatOrElseEither") {
          val value = IO.fail(new Exception()).repeatOrElseEither(Schedule.recurs(2), (_, _: Option[Int]) => ZIO("2"))
          assertM(value)(equalTo(Left("2")))
        }
      ),
      suite("Retry")(
        testM("retry") {
          val value = IO(1).retry(Schedule.recurs(10))
          assertM(value)(equalTo(1))
        },
        testM("retryOrElse") {
          val value =
            (IO.fail(new Exception()): ZIO[Any, Exception, Int])
              .retryOrElse(Schedule.recurs(2), (_, _: Int) => ZIO(2))
          assertM(value)(equalTo(2))
        },
        testM("retryOrElseEither") {
          val value =
            (IO.fail(new Exception()): ZIO[Any, Exception, Int])
              .retryOrElseEither(Schedule.recurs(100), (_, _: Int) => ZIO("2"))
          assertM(value)(equalTo(Left("2")))
        }
      ),
      suite("Base Schedules")(
        testM("forever") {
          val value = IO(1).repeat(Schedule.forever).timeout(Duration.Zero)
          assertM(value)(equalTo(None))
        },
        testM("never") {
          val value = IO(1).repeat(Schedule.never).timeout(Duration.Zero)
          assertM(value)(equalTo(None))
        },
        testM("recurs") {
          val value = IO(1).repeat(Schedule.recurs(5))
          assertM(value)(equalTo(5))
        },
        testM("spaced") {
          val value =
            IO(1).repeat(Schedule.spaced(100.millis)).timeout(Duration.Zero)
          assertM(value)(equalTo(None))
        },
        testM("exponential") {
          val value =
            IO(1).repeat(Schedule.exponential(10.millis)).timeout(Duration.Zero)
          assertM(value)(equalTo(None))
        },
        testM("fibonacci") {
          val value =
            IO(1).repeat(Schedule.fibonacci(10.millis)).timeout(Duration.Zero)
          assertM(value)(equalTo(None))
        }
      ),
      suite("Schedule Combinators")(
        testM("jittered") {
          val value = IO(1).repeat(Schedule.exponential(10.milliseconds).jittered).timeout(Duration.Zero)
          assertM(value)(equalTo(None))
        },
        testM("delayed") {
          val value = IO(1).repeat(Schedule.spaced(1.second).delayed(_ => 100.millis)).timeout(Duration.Zero)
          assertM(value)(equalTo(None))
        },
        testM("sequential") {
          val value = IO(1).repeat(Schedule.recurs(10) andThen Schedule.spaced(1.second)).timeout(Duration.Zero)
          assertM(value)(equalTo(None))
        },
        testM("intersection") {
          val value = TestClock.adjust(1.hour) *> IO(1).repeat(Schedule.spaced(1.second) && Schedule.recurs(10))
          assertM(value)(equalTo((10, 10)))
        },
        testM("union") {
          val value = IO(1).repeat(Schedule.recurs(5) || Schedule.recurs(10))
          assertM(value)(equalTo((5, 10)))
        },
        testM("pipe") {
          val expMaxElapsed = (Schedule.spaced(10.millis) >>> Schedule.elapsed).whileOutput(_ < 30.seconds)
          val value = TestClock.adjust(1.hour) *> IO(1).repeat(expMaxElapsed)
          assertM(value)(equalTo(30.second))
        },
        testM("doWhile") {
          import scala.concurrent.TimeoutException

          val whileTimeout = Schedule.exponential(10.millis) && Schedule.doWhile[Throwable] {
            case _: TimeoutException => true
            case _                   => false
          }
          val ex = new Exception("")
          val value = TestClock.adjust(1.hour) *> IO(ex).repeat(whileTimeout)
          assertM(value)(equalTo((10.millis, ex)))
        }
      )
    )
}
