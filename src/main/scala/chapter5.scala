
object chapter5 {

  import Stream._
  trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

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
      case Cons(h,t) if(f(h())) => Stream.cons(h(), t() takeWhile f)
      case _ => Stream.empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)


    //Exercise 4
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a,b) => p(a) && b)

    //Exercise 5
    def takeWhile2(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((a,b) => if(p(a)) cons(a, b) else b)

    //Exercise 6
    def headOption2: Option[A] =
      foldRight(None: Option[A])((h,_) => Some(h))

    //Exercise 7
    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((h,acc) => cons(f(h), acc))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h,acc) => if(f(h)) cons(h, acc) else acc)

    def append[B >:A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h,acc) => cons(h, acc))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((h,acc) => f(h) append acc)

    def find(p: A => Boolean): Option[A] =
      filter(p).headOption

    //Exercise 13
    def mapViaUnfold(f: A => B) =
      unfold(this) {
        case Cons(h,t) => Some((f(h()), t()))
        case _ => None
      }

    def takeViaUnfold(n: Int): Stream[A] =
      unfold((this,n)) {
        case (Cons(h,t), n) if n > 0 => Some((h(), (t(), n-1)))
        case _ => None
      }
    
    def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
      unfold(this) {
        case Cons(h,t) if f(h()) => Some((h(), t()))
        case _ => None
      }

    def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
      unfold((this, s2)) {
        case (Cons(h,t), Cons(h1,t1)) => Some((f(h(),h1()), (t(), t1())))
        case _ => None
      }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
      zipWithAll(s2)((_,_))
    
    /*
     There are a number of edge cases with this function. We can deal with some of these edge cases by treating each stream as an infinite series of `Option` values, which become `None` when the stream is exhausted. 
     */
    def zipWithAll[B,C](s2: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] = {
      val a = this map (Some(_)) append (constant(None))
      val b = s2 map (Some(_)) append (constant(None))
      unfold((a, b)) {
        case (Empty, Empty) => None
        case (s1, s2) => for {
          h1 <- s1.headOption
          h2 <- s2.headOption
        } yield (f(h1,h2), (s1 drop 1, s2 drop 1))
      }
    }

    //Exercise 14


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

    val ones: Stream[Int] = Stream.cons(1, ones)

    //Exercise 8
    def constant[A](a: A): Stream[A] = {
      lazy val tail: Stream[A] = Cons(() => a, () => tail)
      tail
    }

    //Exercise 9
    def from(n: Int): Stream[Int] = 
      cons(n, from(n+1))

    //Exercise 10
    def fibs: Stream[Int] = {
      def go(n1: Int, n2: Int): Stream[Int] = cons(n1, go(n2, n1+n2))
      go(0,1)
    }

    //Exercise 11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case Some((a,s)) => cons(a, unfold(s)(f))
        case None => empty
      }
    }

    //Exercise 12
    val fibsViaUnfold =
      unfold((0,1)){case (x,y) => Some((x,(x,x+y)))}

    def fromViaUnfold(n: Int) =
      unfold(n)(x => Some((x, x+1)))

    def constantViaUnfold[A](a: A) =
      unfold(a)(x => Some((x, x)))

    val onesViaUnfold = unfold(1)(_ => Some((1,1)))


  }

}
