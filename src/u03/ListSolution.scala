package u03

import u03.Lists.List._


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



}

