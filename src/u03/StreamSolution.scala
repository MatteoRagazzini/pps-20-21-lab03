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

}
