def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]
  = a.map(f.curried).flatMap(b.map)
