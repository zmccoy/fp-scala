
object chapter12 {

  trait Applicative[F[_]] extends Functor[F] {
    // primitive combinators
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    def unit[A](a: => A): F[A]
    // derived combinators
    def map[B](fa: F[A])(f: A => B): F[B] =
      map2(fa, unit(()))((a, _) => f(a))
    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]]
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

    def sequence[A](fas: List[F[A]]): F[List[A]] =
      fas.foldRight(unit(List.empty[A]))((a,b) => map2(a,b)(_ :: _))

    def sequence2[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(unit)

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      traverse((1 to n))(_ => fa)

    def product[A,B](fa: F[A], fb: F[A]): F[(A,B)] =
      map2(fa,fb)((_,_))
  }


  trait ApplicativeApply[F[_]] extends Functor[F] {
    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]
    def unit[A](a: => A): F[A]

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[A] =
      apply(map(fb)(b => ((a: A) => f(a,b))))(fa)

   // def applyWithMap2Unit[A,B](fab: F[A => B])(fa: F[A]): F[B] =
   //   map2(fab, fa)((ab, a) => ab(a))

    def map3[A,B,C,D](fa: F[A],
                      fb: F[B],
                      fc: F[C])(f: (A, B, C) => D): F[D] = {
      apply(
        apply(
          apply(
            unit(f.curried)
          )(fa)
        )f(b)
      )f(c)
    }

    def map4[A,B,C,D,E](fa: F[A],
                        fb: F[B],
                        fc: F[C],
                        fd: F[D])(f: (A, B, C, D) => E): F[E] = {
      apply(
        apply(
          apply(
            apply(
              unit(f.curried)
            )(fa)
          )(fb)
        )(fc)
      )(fd)
    }
  }



}
