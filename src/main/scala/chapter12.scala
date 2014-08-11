
object chapter12 {

  trait Applicative[F[_]] extends Functor[F] {
    // primitive combinators
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    def unit[A](a: => A): F[A]
    // derived combinators
    def map[A,B](fa: F[A])(f: A => B): F[B] =
      map2(fa, unit(()))((a, _) => f(a))
    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]]
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

    def sequence[A](fas: List[F[A]]): F[List[A]] =
      fas.foldRight(unit(List.empty[A]))((a,b) => map2(a,b)(_ :: _))

    def sequence2[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(unit)

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      traverse((1 to n))(_ => fa)

    def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
      map2(fa, fb)((_,_))

    def assoc[A,B,C](p: (A,(B,C))): ((A,B), C) =
      p match { case (a, (b, c)) => ((a,b), c) }

    def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
      ofa.foldRight(unit(Map.empty[K,V])){(kfv,acc) =>
        val (k,fv) = kfv
        map2(fv, fm)((v,m) => m + (k -> v))
      }

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



  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A): Either[E,A] = Right(a)
    def flatMap[A,B](fa: Either[E,A])(f: A => Either[E,B]): Either[E, B] = fa match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }

  sealed trait Validation[+E, +A]
  case class Failure[E](head: E, tail: Vector[E] = Vector())
      extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  def validationApplicative[E] = new Applicative[({type z[A] = Validation[E,A]})#z] = {
    def unit[A](a: => A): Validation[E,A] = Success(a)
    def map2[A,B,C](fa: Validation[E,A], fb: Validation[E,B])(f: (A, B) => C): Validation[E,C] =  (fa,fb) match {
      case (Failure(h1,t1), Failure(h2,t2)) => Failure(h1, t1 ++ (h2 +: t2))
      case (fail@Failure(h1,t1), Success(a)) => fail
      case (Success(a), fail@Failure(h2,t2)) => fail
      case (Success(a), Success(a2)) => unit(f(a,a2))
    }
  }


  import java.util.Date
  case class WebForm(name: String, birthDate: Date, phoneNumber: String)

  def validName(name: String): Validation[String, String] =
    if (name.nonEmpty) Success(name) else Failure("empty name")

  def validBirthDate(birthDate: String): Validation[String, Date] =
    try {
      import java.text.SimpleDateFormat
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthDate))
    } catch {
      case e: Throwable => Failure("Birth date must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber) else Failure("phone number must be 10 digits")

  def validWebForm(name: String, birthDate: String, phoneNumber: String): Validation[String, WebForm] =
    validationApplicative[String].map3(validName(name),
                                       validBirthDate(birthDate),
                                       validPhone(phoneNumber))(WebForm(_,_,_))

  def assoc[A,B,C](p: (A,(B,C))): ((A,B),C) = p match {
    case (a,(b,c)) => ((a,b),c)
  }

  def productF[I,O,I2,O2](f: I => O, g: I2 => O2): (I,I2) => (O,O2) =
    (i, i2) => (f(i), g(i2))


  def productApplicative[F[_], G[_]](F: Applicative[F], G: Applicative[G]): Applicative[({type f[x] = (F[x],G[x])})#f] =
    new Applicative[({type f[x] = (F[x],G[x])})#f] {
      def map2[A,B,C](fa: (F[A],G[A]), fb: (F[B], G[B]))(f: (A,B) => C): (F[C], G[C]) =
        (F.map2(fa._1, fb._1)(f),G.map2(fa._2, fb._2)(f))
      def unit[A](a: => A): (F[A], G[A]) =
        (F.unit(a), G.unit(a))
    }

  def compose[F[_], G[_]](F: Applicative[F], G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def map2[A,B,C](fa: F[G[A]], fb: F[G[B]])(f: (A,B) => C): F[G[C]] =
        F.map2(fa, fb)((ga,gb) => G.map2(ga,gb)(f))
      def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
    }


  trait Traverse[F[_]] {
    def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
      sequence(map(fa)(f))
    def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] =
      traverse(fga)(ga => ga)
  }


  case class Tree[+A](head: A, tail: List[Tree[A]])

  object Traverse {
    val listTraverse = new Traverse[List] {
      override def traverse[M[_],A,B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
        as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)(_ :: _))
    }

    val optionTraverse = new Traverse[Option] {
      override def traverse[M[_],A,B](oa: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
        oa match {
          case Some(a) => M.map(f(a))(Some(_))
          case None    => M.unit(None)
        }
    }

    val treeTraverse = new Traverse[Tree] {
      override def traverse[M[_],A,B](ta: Tree[A])(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] =
        M.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
    }
  }






  }
