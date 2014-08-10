
trait chapter11 {
  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]

    def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))

    def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
  }

  object Functor {
    val listFunctor = new Functor[List] {
      def map[A,B](as: List[A])(f: A => B): List[B] = as map f
    }
  }


  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
    def map[A,B](ma: F[A])(f: A => B): F[B] =
      flatMap(ma)(a => unit(f(a)))
    def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    def sequence[A](lma: List[F[A]]): F[List[A]] =
      lma.foldRight(unit(List[A]()))((a,b) => map2(a,b)(_ :: _))

    def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
      la.foldRight(unit(List[A]()))((a,b) => map2(f(a), b)(_ :: _))

    def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      sequence(List.fill(n)(ma))

    def replicateMRec[A](n: Int, ma: F[A]): F[List[A]] =
      if (n <= 0) unit(List[A]()) else map2(ma, replicateM(n - 1, ma))(_ :: _)

    def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
      if(ms.isEmpty)
        unit(List[A]())
      else {
        val h = ms.head
        val t = ms.tail
        flatMap(f(head)){b =>
          if(b)
            map(filterM(tail)(f))(head :: _)
          else
            filterM(tail)(f)
        }
      }

    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
      (a => flatMap(f(a))(b => g(b)))

    //identity => apply => applyfunction
    def flatMap_[A,B](ma: F[A])(f: A => F[B]): F[B] =
      compose[Unit, A,B](_ => ma, f)(())

    def join[A](mma: F[F[A]]): F[A] =
      flatMap(mma)(identity)

    def flatMap_3[A,B](ma: F[A])(f: A => F[B]): F[B] =
      join(map(f)(ma))
  }

  object Monad {
    val genMonad = new Monad[Gen] {
      def unit[A](a: => A): Gen[A] = Gen.unit(a)
      def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
        ma flatMap f
    }

    val parMonad = new Monad[Par] {
      def unit[A](a: => A): Par[A] = Par.unit(a)
      def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
    }

    val listMonad = new Monad[List] {
      def unit[A](a: => A): List[A] = List(a)
      def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
    }

    val optionMonad = new Monad[Option] {
      def unit[A](a: => A): Option[A] = Some(a)
      def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
    }

    val idMonad = new Monad[Id] {
      def unit[A](a: => A): Id[A] = Id(a)
      def flatMap[A,B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
    }

    def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
      def unit[A](a: => A): State[S,A] = State(s => (a, s))
      def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] =
        st flatMap f
    }

  }


  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  case class State[S, A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      State(s => {
              val (a, s1) = run(s)
              (f(a), s1)
            })
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
              val (a, s1) = run(s)
              f(a).run(s1)
            }) }

  val F = stateMonad[Int]
  def zipWithIndex[A](as: List[A]): List[(Int,A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc,a) =>
      for {
        n  <- getState
        xs <- acc
        _  <- setState(n + 1)
      } yield (n, a) :: xs).run(0)._1.reverse

  case class Reader[R, A](run: R => A)
  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
      def unit[A](a: => A): Reader[R,A]
      def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B]
    }
  }

}
