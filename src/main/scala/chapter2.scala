//Chapter 2 FP In Scala book

object ChapterTwo {

  //Exercise 1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(pos: Int, prev: Int, cur: Int): Int =
      if (pos == 0) prev
      else go(pos - 1, cur, prev + cur)
    go(n, 0, 1)
  }

  //Exercise 2
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int, prev: A): Boolean = {
      if(n == as.length) true
      else if(gt(as(n), prev)) loop(n + 1, as(n))
      else false
    }
    if(as.length == 0) true
    else loop(1, as(0))
  }

  //Exercise 3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a,b)

  //Exercise 4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a,b) => f(a)(b)

  //Exercise 5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))




}
