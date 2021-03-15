package u02

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u03.ListSolution.drop
import u03.Lists.List.{Cons, Nil}

class ListTest {

  val lst = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def dropTest(): Unit ={
     assertEquals(Cons(20, Cons(30, Nil())), drop(lst, 1))
     assertEquals(Cons(30, Nil()), drop(lst, 2))
     assertEquals(Nil(), drop(lst, 5))
  }

}
