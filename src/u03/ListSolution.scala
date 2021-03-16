package u03

import u02.Optionals.Option
import u02.Optionals.Option.{None, Some}
import u03.Lists.List._
import u02.SumTypes._


object ListSolution {
  import Lists._


  def drop[A](l:List[A], n:Int):List[A] = l match{
    case Cons(_,t)  if n>0 => drop(t, n-1)
    case Cons(h,t) if n<=0 => Cons(h,t)
    case Nil() => Nil()
  }

  def flatMap[A,B](l: List[A])(mapper: A => List[B]): List[B] = l match{
    case Cons(h,t) => append(mapper(h), flatMap(t)(mapper))
    case _ => Nil()
  }

  @Override
  def map[A,B](l: List[A])(mapper: A=>B): List[B] = flatMap(l)(v=>Cons(mapper(v), Nil()))

  @Override
  def filter[A](l: List[A])(pred: A=>Boolean): List[A] = flatMap(l) {
    case a if pred(a) => Cons(a, Nil())
    case _ => Nil()
  }

  def max(l: List[Int]): Option[Int] =  l match {
    case Cons(h,t) => t match {
      case Cons(_,_) => max(filter(t)(_>h))
      case Nil() => Some(h)
    }
    case _ => None()
  }

  // utility function to get the course taken by a specific teacher
  def course(p: Person): String = p match {
    case Teacher(_,c) => c
  }

  // Firstly filter the list taking only the Teachers and then map each teacher with the corresponding course
  def getCourses(l: List[Person]): List[String] = map(filter(l)(a => a.isInstanceOf[Teacher]))( p => course(p))


  def foldRight[A,B](list: List[A])(accumulator: B)(binOp :(A, B) => B ): B = list match {
    case Cons(h,t) => binOp(h, foldRight(t)(accumulator)(binOp))
    case _ => accumulator
  }
}

