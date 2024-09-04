import Predef.identity

def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]]
  = traverse(as)(identity)

def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]]
  = as match
    case Nil => Right(Nil)
    case a :: as =>
      for
        a_ <- f(a)
        as_ <- traverse(as)(f)
      yield a_ :: as_
