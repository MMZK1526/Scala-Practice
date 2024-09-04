def sequence[A](as: List[Option[A]]): Option[List[A]] = as match
  case Nil => Some(Nil)
  case a :: as => map2(a, sequence(as))(_::_)
