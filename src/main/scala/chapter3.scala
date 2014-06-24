

object Chapter3 {

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  //Exercise 1
  //The expression would evaluate to: 3

  //Exercise 2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, t) => t
      case Nil => Nil //Could error out if we wanted to
    }
  }

  //Exercise 3
  def setHead[A](h: A, l: List[A]): List[A] = {
    l match {
      case Cons(h,t) => Cons(h, Cons(h,t))
      case Nil => Cons(h, Nil)
    }
  }

  //Exercise 4
  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Cons(_,t) if(n > 0) => drop(t, n-1) 
      case Cons(h,t) if(n == 0) => Cons(h,t)
      case Nil => Nil
    }
  }

  def dropAgain[A](l: List[A], n:Int): List[A] = {
   if(n <= 0) l
   else l match {
     case Cons(_,t) => dropAgain(t,n-1)
     case Nil => Nil
   }
  }

  //Exercise 5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h,t) if(f(h)) => dropWhile(t, f)
      case _ => l
    }
  }

  //Exercise 6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
      case Nil => sys.error("init of empty")
    }
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  //Exercise 7
  //no it can not because it won't actually eval the * until the very end, fold right needs to process the entire list first

  //Exercise 8
    /* 
  We get back the original list! Why is that? As we mentioned earlier, one way of thinking about what `foldRight` "does" is it replaces the `Nil` constructor of the list with the `z` argument, and it replaces the `Cons` constructor with the given function, `f`. If we just supply `Nil` for `z` and `Cons` for `f`, then we get back the input list. 
  
  foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
  Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
  Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
  Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
  Cons(1, Cons(2, Cons(3, Nil))) 
  */

  //Exercise 9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_,acc) => acc + 1)

  //Exercise 10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match { 
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  //Exercise 11
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3(l: List[Int]): Int = foldLeft(l,0)(_*_)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc,h) => acc + 1)

  //Exercise 12
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc,h) => Cons(h,acc))
  }

  //Exercise 13

  //No more stack overflows
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = {
    foldLeft(reverse(l), z)((acc, h) => f(h,acc))
  }

  /*  Taken from the notes on the answer key
     The other implementations build up a chain of functions which, when called, results in the operations being performed with the correct associativity. We are calling `foldRight` with the `B` type being instantiated to `B => B`, then calling the built up function with the `z` argument. Try expanding the definitions by substituting equals for equals using a simple example, like `foldLeft(List(1,2,3), 0)(_ + _)` if this isn't clear. Note these implementations are more of theoretical interest - they aren't stack-safe and won't work for large lists.
  */

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B = 
    foldRight(l, (b:B) => b)((a,gAcc) => b => gAcc(f(b,a)))(z)

  //Exercise 14
  def append[A](l: List[A], x: List[A]): List[A] =
    foldRight(l, x)(Cons(_,_))

  //Exercise 15
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil:List[A])(append)

  //Exercise 16




}

}
