package u03

import u03.Lists.List._


object ListSolution {
  import Lists._

  def drop[A](l:List[A], n:Int):List[A] = l match{
    case Cons(_,t)  if n>0 => drop(t, n-1)
    case Cons(h,t) if n<=0 => Cons(h,t)
    case Nil() => Nil()
  }


}

