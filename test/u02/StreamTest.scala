package u02

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u03.Lists.List.{Cons, Nil}
import u03.StreamSolution
import u03.Streams._

class StreamTest {

  val s = Stream.take(Stream.iterate (0)(_+1))(10)


  @Test def dropTest(): Unit ={
    assertEquals(Cons(6,Cons(7,Cons(8,Cons(9,Nil())))), Stream.toList(StreamSolution.drop(s)(6)))

  }

}
