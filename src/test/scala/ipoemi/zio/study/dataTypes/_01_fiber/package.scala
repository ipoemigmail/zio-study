package ipoemi.zio.study.dataTypes
import zio.{IO, Task}

package object _01_fiber {
  def fib(n: Int): Task[Long] =
    if (n <= 1) IO.succeed(1)
    else
      for {
        fiber1 <- fib(n - 2).fork
        fiber2 <- fib(n - 1).fork
        v2 <- fiber2.join
        v1 <- fiber1.join
      } yield v1 + v2
}
