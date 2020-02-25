package ipoemi.zio.study.overview

import scala.annotation.tailrec

package object _09_background {
  def succeed[A](a: => A) = Return(() => a)
  def printLine(line: String) = PrintLine(line, succeed(()))
  val readLine = ReadLine(line => succeed(line))

  implicit class ConsoleSyntax[+A](self: Console[A]) {
    def map[B](f: A => B): Console[B] =
      flatMap(a => succeed(f(a)))

    def flatMap[B](f: A => Console[B]): Console[B] =
      self match {
        case Return(value) => f(value())
        case PrintLine(line, next) =>
          PrintLine(line, next.flatMap(f))
        case ReadLine(next) =>
          ReadLine(line => next(line).flatMap(f))
      }
  }

  @tailrec
  def interpret[A](program: Console[A]): A = program match {
    case Return(value) =>
      value()
    case PrintLine(line, next) =>
      println(line)
      interpret(next)
    case ReadLine(next) =>
      interpret(next( /*scala.io.StdIn.readLine()*/ "user1"))
  }
}
