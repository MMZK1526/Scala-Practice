import Predef.identity

def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match
  case Nil => Some(Nil)
  case a :: as => map2(f(a), traverse(as)(f))(_::_)

def sequence[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(identity)
