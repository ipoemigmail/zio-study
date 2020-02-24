package ipoemi.zio.study._08_running_effects

import zio._
import zio.console._
import zio.internal.PlatformLive
import zio.test.Assertion._
import zio.test.{DefaultRunnableSpec, _}

object RunningEffectSpec
    extends DefaultRunnableSpec(
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
          ZIO(assert((), equalTo(())))
        },
        testM("DefaultRuntime") {
          val runtime = new DefaultRuntime {}
          val result: Unit = runtime.unsafeRun(putStrLn("Hello World!"))
          ZIO(assert(result, equalTo(())))
        },
        testM("Custom Runtime") {
          val myRuntime: Runtime[Int] = Runtime(42, PlatformLive.Default)
          val result: Int = myRuntime.unsafeRun(ZIO.environment[Int])
          ZIO(assert(result, equalTo(42)))
        }
      )
    )
