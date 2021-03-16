package u02

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u03.Lists.List.{Cons, Nil}
import u03.StreamSolution._
import u03.Streams
import u03.Streams.Stream._

class StreamTest {

  val s: Streams.Stream[Int] = take(iterate (0)(_+1))(10)


  @Test def dropTest(): Unit ={
    assertEquals(Cons(6,Cons(7,Cons(8,Cons(9,Nil())))), toList(drop(s)(6)))
  }

  @Test def constantTest(): Unit ={
    assertEquals(Cons("x",Cons("x",Cons("x",Cons("x",Cons("x",Nil()))))), toList(take(constant("x"))(5)))
  }

}
