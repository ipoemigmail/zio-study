package ipoemi.zio.study.overview._08_running_effects

import zio._
import zio.console._
import zio.internal.Platform
import zio.test.Assertion._
import zio.test.{DefaultRunnableSpec, _}

object RunningEffectSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] =
    suite("Running Effect")(
      testM("App") {
        //object MyApp extends App {
        //  def run(args: List[String]) =
        //    myAppLogic.fold(_ => 1, _ => 0)

        //  val myAppLogic =
        //    for {
        //      _ <- putStrLn("Hello! What is your name?")
        //      name <- getStrLn
        //      _ <- putStrLn(s"Hello, ${name}, welcome to ZIO!")
        //    } yield ()
        //}
        ZIO(assert(())(equalTo(())))
      },
      testM("DefaultRuntime") {
        val runtime = Runtime.unsafeFromLayer(ZEnv.live)

        val result: Unit = runtime.unsafeRun(putStrLn("Hello World!"))
        ZIO(assert(result)(equalTo(())))
      },
      testM("Custom Runtime") {
        val myRuntime: Runtime[Int] = Runtime(42, Platform.default)
        val result: Int = myRuntime.unsafeRun(ZIO.environment[Int])
        ZIO(assert(result)(equalTo(42)))
      }
    )
}
