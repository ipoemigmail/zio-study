package ipoemi.zio.study._02_creating_effect

import java.io.IOException
import java.net.{ServerSocket, Socket}

import zio.{blocking => _, _}
import zio.blocking._
import zio.duration._

import scala.util.Try
import scala.concurrent.Future
import scala.io.{Codec, Source, StdIn}

object CreatingEffect {
  def main(args: Array[String]): Unit = {
    val s1: UIO[Int] = ZIO.succeed(42)
    val s2: Task[Int] = Task.succeed(42)

    lazy val bigList = (0 to 1000000).toList
    lazy val bigString = {
      val s = bigList.map(_.toString).mkString("\n")
      println("run!")
      s
    }

    val s3: UIO[String] = ZIO.effectTotal(bigString)

    val f1: IO[String, Nothing] = ZIO.fail("Uh oh!")

    val f2: Task[Nothing] = Task.fail(new Exception("Uh oh!"))

    val zoption: ZIO[Any, Unit, Int] = ZIO.fromOption(Some(2))

    val zoption2: ZIO[Any, String, Int] = zoption.mapError(_ => "It wasn't there!")

    val zoption3: ZIO[Any, Unit, Int] = ZIO.fromOption(None)

    val zoption4: ZIO[Any, String, Int] = zoption3.mapError(_ => "It wasn't there!")

    val zeither = ZIO.fromEither(Right("Success!"))

    val zeither2 = ZIO.fromEither(Left("Fail!"))

    val ztry = ZIO.fromTry(Try(42 / 0))

    val zfun: ZIO[Int, Nothing, Int] = ZIO.fromFunction((i: Int) => i * i)

    lazy val future = Future.successful("Hello!")

    val zfuture: Task[String] = ZIO.fromFuture { implicit ec =>
      future.map(_ => "Goodbye!")
    }

    val getStrLn: Task[String] = ZIO.effect(StdIn.readLine())

    def putStrLn(line: String): UIO[Unit] = ZIO.effectTotal(println(line))

    val getStrLn2: IO[IOException, String] = ZIO.effect(StdIn.readLine()).refineToOrDie[IOException]

    val throwTest: IO[Throwable, Unit] =
      ZIO.effect[Unit](throw new Exception("")).refineToOrDie[Exception].catchSome {
        case _: Exception => ZIO.effect(())
      }

    case class User(name: String)
    case class AuthError(cause: String)

    object legacy {
      def login(onSuccess: User => Unit, onFailure: AuthError => Unit): Unit = {
        onSuccess(User("valid name"))
      }
    }

    val login: IO[AuthError, User] =
      IO.effectAsync[AuthError, User] { callback =>
        legacy.login(
          user => callback(IO.succeed(user)),
          err => callback(IO.fail(err))
        )
      }

    val sleeping = effectBlocking(Thread.sleep(Long.MaxValue))

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

    println(new DefaultRuntime {}.unsafeRun(console.putStrLn("start") *> accept(s).timeout(Duration.Zero)))

    def download(url: String): ZIO[Any, Throwable, String] =
      ZIO.bracket(Task.effect(Source.fromURL(url)(Codec.UTF8)))(source => ZIO.effectTotal(source.close)) { source =>
        ZIO.succeed(source.mkString)
      }

    def safeDownload(url: String) = blocking(download(url))
  }
}
