package u03

import u03.Lists.List._
import u02.Optionals.Option.Some
import u02.Optionals.Option.None
import u02.Optionals.Option


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



}

