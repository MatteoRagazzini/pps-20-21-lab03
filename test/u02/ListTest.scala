package u02

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u02.Optionals.Option.{None, Some}
import u02.SumTypes.{Student, Teacher}
import u03.ListSolution._
import u03.Lists.List.{Cons, Nil}


class ListTest {

  val lst = Cons(10, Cons(20, Cons(30, Nil())))
  val personsList = Cons(Student("Matteo", 5), Cons(Teacher("Bravetti", "LCMC"), Cons(Teacher("Ghini", "SO"), Nil())))

  @Test def dropTest(): Unit ={
     assertEquals(Cons(20, Cons(30, Nil())), drop(lst, 1))
     assertEquals(Cons(30, Nil()), drop(lst, 2))
     assertEquals(Nil(), drop(lst, 5))
  }

  @Test def flatMapTest(): Unit ={
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(lst)(v => Cons(v+1, Nil())))
    assertEquals(Cons(11,Cons(12,Cons(21,Cons(22,Cons(31,Cons(32,Nil())))))), flatMap(lst)(v => Cons(v+1, Cons(v+2, Nil()))))
  }

  @Test def mapTest(): Unit ={
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(lst)(_+1))
  }

  @Test def filterTest(): Unit ={
    assertEquals(Cons(10, Nil()), filter(lst)(_<15))
  }

  @Test def maxTest(): Unit ={
    assertEquals(Some(30), max(lst))
    assertEquals(None(), max(Nil()))
  }

  @Test def getCoursesTest(): Unit ={
    assertEquals(Cons("LCMC",Cons("SO", Nil())), getCourses(personsList) )
  }

}
