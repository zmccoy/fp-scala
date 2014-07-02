object chapter5 {

  trait Stream[+A] {


    //Exercise 1
    //Will stack overflow
    def toList: List[A] = this match {
      case Cons(h,t) => h() :: t().toList
      case _ => Nil
    }

    //Tailrec
    def toList2: List[A] = {
      @annotation.tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h,t) => go(t(), h() :: acc)
        case _ => acc
      }
      go(this, Nil).reverse
    }


    /*
     In order to avoid the `reverse` at the end, we could write it using a
     mutable list buffer and an explicit loop instead. Note that the mutable
     list buffer never escapes our `toList` method, so this function is
     still _pure_.
     */
    def toListFast: List[A] = {
      val buf = new collection.mutable.ListBuffer[A]
      @annotation.tailrec
      def go(s: Stream[A]): List[A] = s match {
        case Cons(h,t) =>
          buf += h()
          go(t())
        case _ => buf.toList
      }
      go(this)
    }

    //Exercise 2
    def take(n: Int): Stream[A] = {
      if(n > 0) this match {
        case Cons(h,t) => Stream.cons(h(), t().take(n-1))
        case _ => Stream.empty
      } else
        Stream()
    }

    def drop(n: Int): Stream[A] = {
      @annotation.tailrec
      def go(s: Stream[A], n: Int): Stream[A] =
        if(n <= 0) s
        else s match {
          case Cons(h,t) => go(t(), n - 1)
          case _ => Stream.empty
        }
      go(this, n)
    }

    //Exercise 3
    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h,t) => if(f(h())) cons(h(), t() takeWhile f)
      case _ => empty
    }

    //Exercise 4

  }




  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
  }

}
