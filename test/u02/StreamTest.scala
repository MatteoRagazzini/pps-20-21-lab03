package u02

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u03.Lists.List.{Cons, Nil}
import u03.Lists.List
import u03.StreamSolution._
import u03.{StreamSolution, Streams}
import u03.Streams.Stream._

class StreamTest {

  val s: Streams.Stream[Int] = take(iterate (0)(_+1))(10)
  val fibToEight: List[Int] =  Cons(0,Cons(1,Cons(1,Cons(2,Cons(3,Cons(5,Cons(8,Cons(13,Nil()))))))))


  @Test def dropTest(): Unit ={
    assertEquals(Cons(6,Cons(7,Cons(8,Cons(9,Nil())))), toList(drop(s)(6)))
  }

  @Test def constantTest(): Unit ={
    assertEquals(Cons("x",Cons("x",Cons("x",Cons("x",Cons("x",Nil()))))), toList(take(constant("x"))(5)))
  }

  @Test def fibonacciSeriesTest(): Unit ={
    val fibs = StreamSolution.fibonacci()
    assertEquals(fibToEight, toList(take(fibs)(8)))
  }


}
