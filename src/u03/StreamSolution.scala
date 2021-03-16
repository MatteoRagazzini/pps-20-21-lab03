package u03

import u03.Streams.Stream
import u03.Streams.Stream.{Cons, cons, empty}

import scala.annotation.tailrec

object StreamSolution {


  @tailrec
  def drop[A](stream: Stream[A])(n: Int): Stream[A] = stream match {
    case Cons(_, tail) if n>0 => drop(tail())(n - 1)
    case Cons(head, tail) if n<=0 => cons(head(), tail())
    case _ => empty()
  }

  def constant[A] (elem: A): Stream[A] = cons(elem, constant(elem))

  def fibonacci(): Stream[Int] = {
    def _fib(prev:Int, curr: Int): Stream[Int] = cons(prev, _fib(curr, prev+curr))
    _fib(0, 1)
  }

}
