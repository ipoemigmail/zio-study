package ipoemi.zio.study._02_creation_effect

import java.io.IOException
import java.net.{ServerSocket, Socket}

import zio._
import zio.blocking._
import zio.duration._
import zio.test.Assertion._
import zio.test._

import scala.concurrent.Future
import scala.io.{Codec, Source}
import scala.util.Try

object CreatingEffectSpec
    extends DefaultRunnableSpec(
      suite("CreatingEffect")(
        testM("From Success Values") {
          lazy val bigList = (0 to 1000000).toList
          lazy val bigString = bigList.map(_.toString).mkString("\n")
          assertM(ZIO.succeed(42), equalTo(42))
          assertM(Task.succeed(42), equalTo(42))
          assertM(ZIO.effectTotal(bigString), equalTo(bigString))
        },
        testM("From Failure Values") {
          assertM(ZIO.fail("Uh oh!").run, equalTo(Exit.fail("Uh oh!")))
          assertM(Task.fail(new Exception("Uh oh!")).refineToOrDie[Exception].run, fails[Exception](anything))
        },
        testM("From Scala Values") {
          val zoption1 = ZIO.fromOption(Some(2))
          val zoption2 = ZIO.fromOption(None)
          assertM(zoption1, equalTo(2))
          assertM(zoption2.run, equalTo(Exit.fail(())))
          assertM(zoption2.mapError(_ => "It wasn't there!").run, equalTo(Exit.fail("It wasn't there!")))
          assertM(ZIO.fromEither(Right("Success!")), equalTo("Success!"))
          assertM(ZIO.fromEither(Left("Failed")).run, equalTo(Exit.fail("Failed")))
          assertM(ZIO.fromTry(Try(42 / 0)).refineToOrDie[ArithmeticException].run, fails[ArithmeticException](anything))
          assertM(ZIO.fromFunction((i: Int) => i * i).provide(1), equalTo(1))
          lazy val future = Future.successful("Hello!")
          assertM(ZIO.fromFuture { implicit ec =>
            future.map(_ => "Goodbye!")
          }, equalTo("Goodbye!"))
        },
        testM("From Side-Effects") {
          assertM(ZIO.effect( /*StdIn.readLine()*/ "1"), equalTo("1"))
          assertM(ZIO.effectTotal(println("side effect!")), equalTo(()))
          assertM(ZIO.effect(throw new IOException("")).refineToOrDie[IOException].run, fails[IOException](anything))

          case class User(name: String)
          case class AuthError(cause: String)

          val user = User("1")

          object legacy {
            def login(onSuccess: User => Unit, onFailure: AuthError => Unit): Unit = {
              onSuccess(user)
            }
          }

          val login: IO[AuthError, User] =
            IO.effectAsync[AuthError, User] { callback =>
              legacy.login(
                user => callback(IO.succeed(user)),
                err => callback(IO.fail(err))
              )
            }
          assertM(login, equalTo(user))
        },
        testM("Blocking Synchronous Side-Effects") {
          val sleeping = effectBlocking(Thread.sleep(Long.MaxValue))
          assertM(sleeping.timeout(Duration.Zero), equalTo(None))

          var th: Thread = null

          def accept(l: ServerSocket): ZIO[Blocking, Throwable, Socket] =
            effectBlockingCancelable { th = Thread.currentThread(); l.accept() }(UIO.effectTotal(l.close()))

          class MyServerSocket(port: Int) extends ServerSocket(port) {
            override def close(): Unit = {
              println("close!!!")
              super.close()
            }
          }

          val s = new MyServerSocket(19992)

          assertM(console.putStrLn("start") *> accept(s).timeout(Duration.Zero), equalTo(None))

          def download(url: String): ZIO[Any, Throwable, String] =
            ZIO.bracket(Task.effect(Source.fromURL(url)(Codec.UTF8)))(source => ZIO.effectTotal(source.close)) {
              source =>
                ZIO.succeed(source.mkString)
            }

          def safeDownload(url: String) = zio.blocking.blocking(download(url))

          assertM(download("https://zio.dev/docs/about/about_index"), isNonEmptyString)
          assertM(safeDownload("https://zio.dev/docs/about/about_index"), isNonEmptyString)
        }
      )
    )
